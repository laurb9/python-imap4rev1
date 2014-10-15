#-----------------------------------------------------------------------------
# Copyright (C)2001-2002 InfoStreet, Inc.
#                        Laurentiu Badea lc@infostreet.com
#
# Created On:     Feb 20th, 2001
#-----------------------------------------------------------------------------

"""
IMAP4rev1 (RFC2060) high-level interface
Uses myimaplib for low-level IMAP access.
Local/Remote IMAP accounts.

defines the IMAP4 class
"""

from os import environ
from string import *
import re, mimetools, cStringIO
import myimaplib
from mailbox import Mailbox

VERSION = "$Revision: 1.21 $"[11:][:-2]

# variables to be used externally on class initialization i.e. imap4rev1.LOCAL
LOCAL = 1
REMOTE = 2

# Regular expressions

# allowable mailbox name
# We can potentially allow everything except " and \ (to simplify quoting)
# If we extend the char set we need to check for separator as well
RE_MAILBOX = re.compile(r'^[\w ]+$')
# allowed search string
RE_STRING = re.compile(r'^[^\x00-\x1f\x7f-\xff]+$')
# QUOTED_SPECIALS Formal Syntax element in the RFC
RE_QUOTED_SPECIALS = re.compile(r'(["\\])')
# unescaped quote (may be preceded by escaped escapes)
RE_UNESCAPED_QUOTE = re.compile(r'[^\\](\\\\)*"')
# This matches the atom or a flag (allows initial \)
# but leaves the rest of the string open
RE_ATOM = re.compile(r'^(\\?[^\\(){ \x00-\x1f\x7f"]+)')

# Defined exceptions are all subclasses of imapException
class imapException(Exception): pass

# Argument error, for example invalid mailbox name for create()
class argumentError(imapException): pass

# Error occurred while processing a command. Possibly recoverable
# (i.e. select on a nonexistent mailbox)
class commandError(imapException): pass

# Serious error. Command wasn't understood by server.
class protocolError(imapException): pass

# Server response can't be parsed (understood)
class resultError(imapException): pass


class IMAP4:

    """
    IMAP4: Implements one IMAP (remote or local) connection

    Server replies other than OK will generate exceptions.
    Data returned by the server is parsed into python data structures
    (tuples and dictionaries) as described for each method.

    implemented methods:
        Mailbox methods:
            list( getStatus=0, update=0 )
            lsub( getStatus=0, update=0 )
            (messages, unread, recent) = status( mailbox )
            (messages, unread, recent) = select( mailbox )
            create( base, name )
            delete( mailbox )
            expunge( mailbox )
            subscribe( mailbox )
            unsubscribe( mailbox )
            rename( oldmailbox, newbase, newname )
            select( mailbox )
            getSelected( leafOnly = 0 )
        Message methods:
            search(**criteria) (can do sorting if IMAP supports it)
            msgStructure( messageID )
            msgCopy( message, new_mailbox )
            msgMove( message, new_mailbox )
            msgDelete( message )
            realDelete( message )
            msgStore( mailbox, rfc822message )
            msgRetrieve( message, [part, encoding] )
            msgFlag( message, set )
    """

    def __init__(self, type=LOCAL,
                  maildir="",
                  host="", username="", password=""):

        """
        Open connection to IMAP server.

        Specify maildir if type is imap4rev1.LOCAL
        or host, [username] and password if type is imap4rev1.REMOTE
        (if username is not provided email will be used instead)
        """

        if type == LOCAL:
            self.type = LOCAL
            self.imap = myimaplib.IMAP4(maildir=maildir)

        else:
            self.type = REMOTE
            self.imap = myimaplib.IMAP4(host=host)
            self.imap.login(username, password)

        self.mailboxes = None
        self.inbox = "INBOX"
        self.delimiter = None  # hierarchy delimiter, set below
        self.delimiter_incr = 3  # how many chars does its representation take

        self.selected = ""  # selected mailbox
        self.message_set = None  # message set gotten from a search
        self.header_set = None  # expanded header list for above message set
        self.getStatus = 0  # collect message counts by default ? no.
        self.getSubscribed = 0  # get subscribed mailboxes as well

        # Identify hierarchy delimiter used by server

        result = self.imap.list('""', '""')
        result = self._checkStatus(result)
        self._parseMailboxResponse(result[0])  # will update delimiter

        # Cached message structure
        self.lastUID = None
        self.lastStructure = None


    def close(self):

        "Close IMAP connection. You shouldn't use this object after this."

        try:
            if self.imap:
                self.imap.logout()  # an implicit close() happens if needed
        except AttributeError: pass
        self.imap = None


    def __del__(self):
        self.close()


    ###########################################################################
    # MAILBOX-RELATED METHODS
    ###########################################################################
    # non-existent mailboxes should be ignored if they're 'subscribed'

    def list(self, getSubscribed=None, getStatus=None,
              update=0, noData=0):

        """
        Retrieve mailbox hierarchy from server

        See Mailbox.makeDict() for format of returned data
        getStatus=1 retrieves/updates status info on each mailbox (slower!)
        getSubscribed=1 updates the 'subscribed' field in list (i.e. LIST+LSUB)
        Set update=1 to force requerying server (or you might get cached data)
        noData=1 just updates internal list, does not return anything
        """

        # Simplified list() always searches from the top level

        self._initMailboxList(update)

        # Save value of getStatus/getSubscribed so future list updates
        # (after a rename, delete, etc) will update the info the same way
        # we will keep the value that returns the larger set
        if getStatus and self.getStatus < getStatus:
            self.getStatus = getStatus

        if getSubscribed and self.getSubscribed < getSubscribed:
            self.getSubscribed = getSubscribed

        if not self.mailboxes.getName() or update:

            result = self.imap.list()
            result = self._checkStatus(result)
            self._updateMailboxList(result, 0)

            if self.getSubscribed:
                result = self.imap.lsub()
                result = self._checkStatus(result)
                self._updateMailboxList(result, 1)

            # Update root inbox (namespace)
            self.inbox = self.mailboxes.getName()

            # Fix root inbox if not subscribed
            # if not self.mailboxes.subscribed:
            #     self.subscribe(self.inbox)

        if noData:
            return None
        else:
            return self.mailboxes.makeDict()


    def lsub(self, getStatus=None, update=0, noData=0):
        "Get list of subscribed mailboxes"

        return self.list(getSubscribed=1, getStatus=getStatus,
                          update=update, noData=noData)


    def status(self, mailbox):

        """
        Check status of mailbox (total messages, unseen messages etc)

        Returns (messages, unseen, recent) and updates mailboxes structure
        NOTE: Message count may look wrong if there are \\Deleted messages
        Don't call status() on mailboxes with the NOSELECT flag set.
        """

        count = unseen = recent = 0

        result = self.imap.status(mailbox, "(MESSAGES RECENT UNSEEN)")
        result = self._checkStatus(result)[0]

        # parse result. Example: '"INBOX" (MESSAGES 9 UNSEEN 6)'

        # rindex because the mailbox name may contain "(" as well
        index = rindex(result, "(")
        result = split(result[index + 1:-1])

        key = None
        for item in result:
            if not key:
                key = upper(item)
            else:
                if key == "MESSAGES": count = int(item)
                elif key == "UNSEEN": unseen = int(item)
                elif key == "RECENT": recent = int(item)
                key = None

        if self.mailboxes:
            self.mailboxes.add(self._makeTuple(mailbox), name=mailbox,
                                count=count, unseen=unseen, recent=recent)
        return (count, unseen, recent)


    def select(self, mailbox=""):

        "select specified mailbox, returns its status() "
        "mailbox is in IMAP format i.e. INBOX.Trash"

        self.lastUID = None  # reset cache

        if len(mailbox) == 0:
            mailbox = self.inbox

        result = self.imap.select(mailbox)
        result = self._checkStatus(result)
        self.selected = mailbox
        self.message_set = None  # message set gotten from a search
        self.header_set = None  # expanded header list for above message set
        # count = int(result[0])

        # The server returns the status on SELECT, but somehow the imaplib
        # manages to ignore it, so we need this extra STATUS
        return self.status(mailbox)


    def getSelected(self, leafOnly=0):
        m = self.selected
        if leafOnly:
            return self._makeTuple(m)[-1]
        else:
            return m


    def create(self, base, name=None):

        """
        Create a new mailbox and returns its IMAP name
        If name is not specified then the 'base' mailbox is created

        Example: create( 'INBOX.Received' , 'Old' ) -> 'INBOX.Received.Old'
                 create( 'INBOX.Sent' )             -> 'INBOX.Sent'
        """

        # Create the entire path if needed

        self.list(noData=1)

        mailboxT = self._makeTuple(base)
        if name:
            mailboxT.append(name)

        base = mailboxT[0]
        baseT = [base]

        for name in mailboxT[1:]:
            baseT.append(name)
            base = base + self.delimiter + name
            mailbox = self.mailboxes.getLeaf(baseT)
            if not mailbox:
                # Need to create folder at this level
                # Make sure the name itself contains only allowed characters
                if not RE_MAILBOX.search(name):
                    raise argumentError("Invalid mailbox name " + name)

                result = self.imap.create('"' + base + '"')
                result = self._checkStatus(result)

                self.mailboxes.add(baseT, name=base)

            elif mailbox.flags.has_key("NOSELECT"):
                raise argumentError("Folder %s does not exist!" % base)

        return base


    def delete(self, mailbox):

        """
        Delete an existing mailbox

        Removes all messages in mailbox. Does NOT remove subfolders.
        We require that the mailbox does not have children to avoid problems.
        """

        self.list(noData=1)
        mailboxT = self._makeTuple(mailbox)
        mbox_obj = self.mailboxes.getLeaf(mailboxT)

        if mbox_obj:
            if mbox_obj.children:
                raise argumentError("Mailbox " + mailbox + " has subfolders.")

            self.imap.delete(mailbox)  # ignore error
            self.mailboxes.remove(mailboxT)

        else:
            raise argumentError("Nonexistent mailbox " + mailbox)


    def expunge(self, mailbox):

        "Delete all messages marked as \\Deleted from the mailbox"

        if not self.selected == mailbox:
            self.select(mailbox)

        result = self.imap.expunge()
        result = self._checkStatus(result)

        # result is a "list" of expunged messages


    def subscribe(self, mailbox):

        "Mark mailbox as 'subscribed'"

        # We impose the limitation that we can only do it to existing mailboxes

        self.list(noData=1)

        if not self.mailboxes.getLeaf(self._makeTuple(mailbox)):
            raise argumentError("Nonexistent mailbox: " + mailbox)

        result = self.imap.subscribe(mailbox)
        self._checkStatus(result)

        # Record the modification in self.mailboxes
        self.mailboxes.add(self._makeTuple(mailbox), subscribed=1)


    def unsubscribe(self, mailbox):

        "Unmark subscribed mailbox"

        self.list(noData=1)

        result = self.imap.unsubscribe(mailbox)
        self._checkStatus(result)

        # Record the modification in self.mailboxes
        self.mailboxes.add(self._makeTuple(mailbox), subscribed=0)


    def rename(self, oldmailbox, newbase=None, newname=None):

        """
        Move existing mailbox to new folder, or rename mailbox.

        Returns new imap name.
        Examples:
        rename( 'INBOX.Received', 'INBOX.Old' )        -> 'INBOX.Old.Received'
        rename( 'INBOX.Received', 'INBOX.Old', 'New' ) -> 'INBOX.Old.New'
        rename( 'INBOX.Received', newname = 'New' )    -> 'INBOX.New'
        NOTE: All 'subfolders' are also moved/rename together with their parent
        """

        # Extract the folder name from oldmailbox
        oldmailboxT = self._makeTuple(oldmailbox)
        name = oldmailboxT[-1]

        self.list(getSubscribed=1, noData=1)
        try:
            children = self.mailboxes.getLeaf(oldmailboxT).hasChildren()
        except AttributeError:
            raise argumentError("Nonexistent mailbox: " + oldmailbox)

        if not newbase:
            newbaseT = oldmailboxT[:-1]
        else:
            # Make sure the folder where we want to attach this exists
            newbaseT = self._makeTuple(newbase)
            if not self.mailboxes.getLeaf(newbaseT):
                raise argumentError("Nonexistent mailbox: " + newbase)

        if not newname:
            newname = name

        if not RE_MAILBOX.search(newname) or self.delimiter in newname:
            raise argumentError("Invalid mailbox name " + newname)

        newnameT = newbaseT + [newname]

        if self.mailboxes.getLeaf(newnameT):
            raise argumentError("Mailbox already exists: " + newname)

        newname = join(newnameT, self.delimiter)

        # NOTE: courier-imap seems to interpret mailbox names differently
        # in that you can RENAME "mailbox." and it will rename its children
        # but not the mailbox (use RENAME mailbox for that).

        result = self.imap.rename(oldmailbox, newname)
        # hmm, imapd seems to return OK unless the target already exists
        self._checkStatus(result)
        if self.mailboxes.getLeaf(oldmailboxT).subscribed:
            self.mailboxes.copy(oldmailboxT, newnameT, newname)
            self.unsubscribe(oldmailbox)
            self.subscribe(newname)
        self.status(newname)

        if children:
            # Since IMAP RENAME does NOT affect children names we need to
            # recursively rename/move all children now
            for child in children.keys():
                src = oldmailbox + self.delimiter + child
                self.rename(src, newname, str(child))

        self.mailboxes.remove(oldmailboxT)
        # self.list(update=1)

        return newname


    ###########################################################################
    # MESSAGE-RELATED METHODS
    ###########################################################################
    # WARNING: they all REQUIRE a mailbox already select()-ed
    # All these methods work with UIDs to keep references valid over different
    # sessions (they don't, however, check UIDVALIDITY)


    def search(self, search=None, **criteria):

        """
        Search for messages in current mailbox

        Returns a tuple with message headers (see the headers() method
        elsewhere for the format of the header set).

        Don't specify any criteria to get a listing of all messages.
        This does not implement all the search options in IMAP4rev1
        You can also set the SORT criteria to request sorting.
        Possible values: [REVERSE] ARRIVAL CC DATE FROM SIZE SUBJECT TO

        Examples:
        search()
        search( FROM='lc', NOTSUBJECT = 1, SUBJECT = 'imap' )
        (all messages containing 'lc' in the From: header but not having
         'imap' in the subject)
        search( SORT='REVERSE DATE' )
        """

        if not self.selected:
            raise argumentError("No mailbox selected for search")

        query = []  # query elements that are ANDed together
        or_query = []  # query elements that are ORed together

        # Check criteria and build query

        # string-argument options. Negated if "notoption" is 1.
        for key in ("FROM", "TO", "CC", "BCC", "SUBJECT", "BODY"):
            if search:
                or_query.append(self._simpleQuery(key, search))

            elif criteria.has_key(key):
                query.append(self._simpleQuery(key, criteria[key],
                                                  criteria.get("NOT" + key, 0)))


        # Combine string search options into a single query if there
        # are alternative OR statements. The result will be ANDed with
        # the next criteria below.
        # IMAP uses prefixed binary logic (i.e. "OR (OR c1 c2) c3" etc)
        if or_query:  # we know there are more than 1 element
            c1 = or_query.pop()
            while or_query:
                c2 = or_query.pop()
                c1 = "OR (%s) %s" % (c1, c2)

            query.append(c1)

        # date-argument options.
        # IGNORE for now (until we can check/convert date format to DD-MON-YYYY)
        for key in ("BEFORE", "ON", "SINCE",
                     "SENTBEFORE", "SENTON", "SENTSINCE"):
            if criteria.has_key(key):
                pass

        # no argument options
        for key in ("NEW", "OLD", "FLAGGED", "SEEN", "RECENT", "ANSWERED",
                     "DELETED"):
            if criteria.has_key(key):
                if criteria[key] == 0:
                    query.append("NOT " + key)
                else:
                    query.append(key)

        if len(query) == 0:
            query.append("ALL")  # must have something there

        # Exclude \Deleted messages if the UNDELETED criteria is not present
        if not criteria.has_key("DELETED"):
            query.append("NOT DELETED")

        query = "(" + join(query, " ") + ")"

        # See if we want the results sorted.
        # If we do, try to use the SORT extension if available, otherwise
        # it gets more complicated because we need to get the headers
        sort = None
        result = None
        if criteria.has_key("SORT"):  # We want the results sorted

            if "SORT" in self.imap.capabilities:
                result = self.imap.sort(criteria=query,
                                         sort="(" + criteria["SORT"] + ")")
            else:
                sort = criteria["SORT"]  # signal that we need "manual" sorting
                result = self.imap.uid("SEARCH", query)
        else:
            result = self.imap.uid("SEARCH", query)

        result = self._checkStatus(result)[0]

        # result is now a string with numbers: '3 6 7 10'
        # Make it a tuple. We don't convert to ints because as far as we
        # are concerned we don't care what is in there

        if sort:
            return self._sort(split(result), sort)
        else:
            return self.headers(split(result))


    def headers(self, message_set):

        "Retrieve headers for a message set"

        # Sample headers element (generated by _parseFetch):
        # {
        #   envelope_data,
        #   'FLAGS':        {'Seen': 1, 'Deleted': 1 },
        #   'INTERNALDATE': '07-Feb-2001 18:45:28 -0800',
        #   'RFC822.SIZE':  2013,
        #   'UID':          3
        # }

        # envelope_data adds the following fields:
        # DATE, SUBJECT, FROM, SENDER, REPLYTO, TO, CC, BCC, INREPLYTO, MSGID
        # Header fields are tuples with email addresses
        # Note that envelope DATE is in RFC822 format

        # Save message_set and headers list for future reference
        if type(message_set) == type("string"):
            message_set = (message_set,)
        self.header_set = self.fetch(message_set, "ALL")
        self.message_set = message_set

        return self.header_set


    def msgStructure(self, message):

        """Return message structure for this message UID

        message structure returned as follows:
        simple message is a dictionary as described in _parseMsgSimple()
        multipart message is a list containing the subtype then the body parts
        """

        if self.lastUID == message:
            return self.lastStructure

        # BODY.PEEK does not mark the message as \Seen
        data = self.fetch((message,), "BODY.PEEK")

        if not data or not data[0]:
            raise argumentError("Message UID %s does not exist!" % message)

        structure = self._parseMsg(data[0]["BODY"])
        self.lastUID = message
        self.lastStructure = structure
        return structure


    def fetch(self, message_set, message_parts):
        "Generic FETCH. message_set is a tuple or list!."

        "Does not update self.header_set."

        "Returns a list of dictionaries"
        "Don't use to fetch body parts though, see comments below."

        if len(message_set):
            result = self.imap.uid("FETCH", join(message_set, ","),
                                    "(" + message_parts + ")")
            result = self._checkStatus(result)
        else:
            result = ()

        data = []
        for line in result:

            # imaplib splits the result if it finds any literals but it also
            # incorrectly closes the current tuple right after the literal
            # body so the rest of the line comes as the next "response".
            # This will definitely screw up our parsing so don't request
            # things that can return literals (like BODY parts and such)
            # We kludge things here for when the literal is the last element
            if type(line) == type(()):
                line = join(line, "\r\n")
            # Then next line could be ")" and we need to skip it.
            elif line == ')':
                continue

            parsed = self._parseFetch(line)

            # If we have an ENVELOPE here try to "humanize" it
            # We also merge the envelope fields in the main dict because
            # I think there are no key name conflicts
            if parsed:
                if parsed.has_key("ENVELOPE"):
                    new_envelope = self._fixEnvelope(parsed["ENVELOPE"])
                    parsed.update(new_envelope)
                    del parsed["ENVELOPE"]

            data.append(parsed)

        return data


    def msgFlag(self, message, set=None, flag="Flagged"):

        "Get/Set user flag (Flagged) on message"

        # You need to see the RFC if you want to change other flags

        if set != None:
            flags = "+FLAGS.SILENT"
            if set == 0:
                flags = "-FLAGS.SILENT"

            result = self.imap.uid("STORE", message, flags, "\\%s" % flag)
            result = self._checkStatus(result)
            return set

        else:
            result = self.fetch((message,), "FLAGS")[0]
            return result["FLAGS"].has_key(flag)


    def msgCopy(self, message, new_mailbox):

        "Copy a message (specified by UID) to another mailbox"

        result = self.imap.uid("COPY", message, new_mailbox)
        result = self._checkStatus(result)


    def msgMove(self, message, new_mailbox):

        "Move a message (specified by UID) to another mailbox"

        self.msgCopy(message, new_mailbox)
        self.realDelete(message)


    def msgDelete(self, message):

        "Move the message to Trash"

        self.list(noData=1)
        trash = self.mailboxes.getLeaf((self.inbox, "Trash"))
        if not trash:
            # Trash does not exist, try to create it
            trash = self.create(self.inbox, "Trash")

        self.msgMove(message, str(trash))


    def realDelete(self, message):

        "Mark the message as deleted (will be deleted on logout or expunge)"

        result = self.imap.uid("STORE", message, "+FLAGS", "(\\Deleted)")
        result = self._checkStatus(result)


    def msgStore(self, mailbox, rfc822message):

        "Store RFC822 message in mailbox"

        result = self.imap.append(mailbox, None, None, rfc822message)
        self._checkStatus(result)
        return self.status(mailbox)


    def msgRetrieve(self, message, part="RFC822", encoding=None):

        """Retrieve a full message or a message part for this UID

        You can get a message part if you specify the part id
        by default it returns the whole RFC822 part of it
        Although theoretically it could fetch multiple parts, imaplib
        massacrates the result so it's difficult to parse.
        Specify encoding type to get the data back decoded
        """

        # Convert part number to IMAP-style "BODY[part]"
        if '0' < part[0] <= '9':
            part = "BODY[%s]" % part

        result = self.imap.uid("FETCH", message, "(" + part + ")")
        result = self._checkStatus(result)

        if not result[0]:
            return None

        data = result[0][1]  # imaplib split the returned literal
        # see if we need to decode it.
        if encoding in ('base64', 'quoted-printable', 'uuencode'):
            output = cStringIO.StringIO()
            input = cStringIO.StringIO(data)
            mimetools.decode(input, output, encoding)
            input.close()
            data = output.getvalue()
            output.close()

        return data


    ###########################################################################
    # INTERNAL METHODS
    ###########################################################################
    # Mostly parsers for the various IMAP4 response string sequences
    # and various helper methods

    def _makeTuple(self, mailbox):

        "Make a tuple out of a mailbox name"

        return split(mailbox, self.delimiter)


    def _joinTuple(self, mailboxT):

        "Make an IMAP name out of a tuple"

        return join(mailboxT, self.delimiter)


    def _initMailboxList(self, init=0):

        "Initialize mailboxes list if needed or requested (init=1)"

        if init or not self.mailboxes:
            self.mailboxes = Mailbox()


    def _updateMailboxList(self, result, subscribed):

        "update self.mailboxes with data from an imaplib list() or lsub()"

        for mailbox in result:
            if not mailbox:
                break

            mailbox, flags = self._parseMailboxResponse(mailbox)
            mailboxT = self._makeTuple(mailbox)

            # We will only check status on subscribed mailboxes if needed
            if (self.getStatus and subscribed and
                 not flags.has_key("NOSELECT")):
                # Subscribed mailboxes need not exist according to RFC
                # we are going to execute the status() anyway and set
                # the "NOSELECT" flag if it failed, so the names with
                # getStatus=1 are the same as the one with it on 0.
                try:
                    self.status(mailbox)
                except commandError:
                    flags["NOSELECT"] = 1

            # I kind of assume here that we already did a LIST()
            if subscribed and not self.mailboxes.getLeaf(mailboxT):
                flags["NOSELECT"] = 1

            self.mailboxes.add(mailboxT,
                                name=mailbox,
                                flags=flags,
                                subscribed=subscribed)


    def _checkStatus(self, result):

        """Check the status of an IMAP operation, return data part of result

        Used after each call to a self.imap method"""

        if result[0] == "NO":
            raise commandError("IMAP command failed: " + result[1][0])
        elif result[0] != "OK":  # "BAD"
            raise protocolError("IMAP protocol error: " + result[1][0])

        # See if we have any additional data that was sent by the server
        # see resp_text in rfc.

        extra = self.imap.response("OK")[1]
        for line in extra:
            if line and line[0:7] == "[UNSEEN":
                self.unseen = int(line[9:-1])

        return result[1]


    def _parseMailboxResponse(self, response):

        """Parse a mailbox line returned by the IMAP server

        Used after a LIST/LSUB in list()"""

        # Syntax (see mailbox_list in RFC): (flags)
        # Format example: (\Unmarked \HasChildren) "." "INBOX"
        # Mailbox attributes:
        # \Noinferiors \Noselect \Marked \Unmarked ...

        # NOTE: Should use regular expressions, maybe

        flagstring, pos = self._getPList(response)
        flags = self._parseFlags(flagstring)
        pos = pos + 1  # skip the space

        # See if we need to extract the delimiter
        if not self.delimiter:
            if upper(response[pos:pos + 3]) != 'NIL':  # must be quoted delimiter
                if response[pos + 1] == '\\':  # escaped delimiter
                    self.delimiter_incr = 4
                    self.delimiter = response[pos + 2]
                else:
                    self.delimiter = response[pos + 1]
            else:
                pass  # we won't be able to use hierarchies

        # skip the mandatory space after ")" plus the whole delimiter
        mailbox = response[pos + 1 + self.delimiter_incr:]

        return (self._getString(mailbox)[0], flags)


    def _getString(self, astring):

        """extract the string from atomic, quoted or literal form

        returns a tuple where the first element is the extracted string
        and the second is the position after the end of it (so we can
        continue parsing the input string)
        """

        trimmed = len(astring)
        astring = lstrip(astring)
        trimmed = trimmed - len(astring)

        if astring[0] == '{':  # literal: "{" size "}" CR LF data
            pos = index(astring, "}\r\n")
            size = int(astring[1:pos])
            return (astring[pos + 3, pos + 3 + size], pos + 3 + size + trimmed)

        elif astring[0] == '"':  # quoted.
            # The only quoted_specials allowed are " and \

            # Find the position of the end-of-string quote
            found = RE_UNESCAPED_QUOTE.search(astring)
            if not found:
                # should return exception - data is malformed probably
                return (None, 1 + trimmed)

            pos = found.end() - 1  # the double quote is the last char
            string = astring[1:pos]

            # unescape escape sequences
            string = replace(string, r'\\', '\\')  # \\ -> \
            string = replace(string, r'\"', '"')  # \" -> "
            return (string, pos + 1 + trimmed)

        else:
            found = RE_ATOM.search(astring)
            if not found:
                # Should raise an exception...
                # We add one to avoid getting in an infinite loop
                return (None, 1 + trimmed)

            pos = found.end()

            string = found.group(1)

            if upper(string) == "NIL":
                return (None, pos + trimmed)
            else:
                return (astring[:pos], pos + trimmed)


    def _getPList(self, plist, start=1):

        "make a tuple structure from a parenthesized list (i.e. envelope)"

        tuple = []

        l = len(plist)
        i = start  # first char is a "("

        while i < l:

            if plist[i] == '(':  # nested, process recursively
                (data, offset) = self._getPList(plist, i + 1)
                tuple.append(data)
                i = offset

            elif plist[i] == ')':  # our level is closed, return the data
                return (tuple, i + 1)

            elif plist[i] == ' ':
                i = i + 1

            else:
                (string, offset) = self._getString(plist[i:])
                tuple.append(string)
                i = i + offset

        # We should not get here (it means the list wasn't closed)
        return (tuple, l)


    def _simpleQuery(self, key, arg, negate=0):

        "Return a query element doing the necessary sanity checks on arg"

        "Used by search()"

        if negate:
            key = "NOT " + key

        if arg:
            if not RE_STRING.search(arg):
                raise argumentError("Invalid search string for " + key)

            # Escape quoted_specials (double quotes and escape char)
            arg = RE_QUOTED_SPECIALS.sub(r'\\\1', arg)
            return '%s "%s"' % (key, arg)

        else:
            return key


    def _sort(self, message_set, sortby):

        """
        Sort a message set based on given criteria and return the header set.

        used by search()
        Sort criteria: one or two of: ARRIVAL CC DATE FROM SIZE SUBJECT TO
        REVERSE can prefix any of them.
        See the headers() method elsewhere for the format of the header set.
        """
        # ## STUB (NOT IMPLEMENTED)

        # see if we already have the headers
        if not (self.header_set and
                self.message_set and self.message_set == message_set):
            headers = self.headers(message_set)

        return headers

        # NOTE: Should store the headers in case they are requested later


    def _parseFetch(self, line):

        """Parse a FETCH response, fill out a dictionary with the fields

        Used after a fetch() call"""

        # Parsing the response is difficult because of literals and
        # parenthesized lists. A lexical parser would probably work great here.

        if not line:
            return None

        try:
            pos = index(line, " ")  # First chars are: number SPACE "("
        except ValueError:
            raise resultError("Can't parse FETCH result '" + line[:64] + "'")

        pos = pos + 2  # skip them.

        line = line[pos:-1]

        data = {}

        done = 0
        while len(line) > 0 and not done:
            (field, line) = split(line, None, 1)
            field = upper(field)

            if   field in ("UID", "RFC822.SIZE"):  # number
                (num, pos) = self._getString(line)
                data[field] = int(num)

            elif field in ("INTERNALDATE", "RFC822.HEADER", "RFC822.TEXT"):
                # nstring ::= quoted / literal / "NIL"
                (data[field], pos) = self._getString(line)

            elif field in ("ENVELOPE", "BODY", "BODYSTRUCTURE"):
                (data[field], pos) = self._getPList(line)

            elif field == "FLAGS":
                (temp, pos) = self._getPList(line)
                data[field] = self._parseFlags(temp)

            else:
                print "Don't know how to handle %s tag." % (field)
                done = 1  # for now

            line = line[pos:]

        return data


    def _parseParameters(self, params):
        """Parse body parameter list

        Used in _parseMsgSimple()"""

        parsed = {}
        key = None
        if params:
            for elem in params:
                if not key:
                    key = elem
                else:
                    parsed[key] = elem
                    key = None

        return parsed


    def _parseMsgSimple(self, data, part):
        """
        return message structure dictionary for a simple message given in data:

        { 'part':         '1',    # MIME part identifier
          'type':         'text',
          'subtype:       'plain', # type/subtype is a MIME type
          'param':        ['charset', 'iso-8859-1'],
          'id':           None,
          'description':  None,
          'encoding':     '7bit', # '8bit','binary','base64','quoted-printable'
          'size':         330,
          # if type is 'text' or 'message'
          'lines':        17,
          # if type/subtype is 'message/rfc822':
          'envelope':     envelope_data,
          'body':         { another simple message structure }
        }
        """

        # data looks like this:
        # ['text', 'plain', [param_data], None, None, '7bit', '330', '17']
        # (an rfc822/message will contain envelope and body before no of lines)

        body = {
            'part':           part,
            'type':           data[0],
            'subtype':        data[1],
            'param':          self._parseParameters(data[2]),
            'id':             data[3],
            'description':    data[4],
            'encoding':       data[5],
            'size':           data[6]
        }

        if body['type'] == 'message' and body['subtype'] == 'rfc822':
            # [7] is envelope, [8] is body, [9] is lines
            body['envelope'] = self._fixEnvelope(data[7])
            body['body'] = self._parseMsg(data[8], part)
            body['lines'] = data[9]

        elif len(data) >= 8:
            body['lines'] = data[7]

        return body


    def _parseMsg(self, data, part=""):
        """Parse a multipart or simple message's structure

        called by msgStructure()"""

        # multipart data looks like this (empirically from courier because
        # I can't make it match the body and body_type_mpart specs).
        # [ [body], [body], [...], subtype] ]
        # (subtype='mixed', 'alternative', 'type' is assumed 'multipart')

        # if the first element of the body array is another array then
        # we have a multipart message (the last element is the subtype)
        if type(data[0]) == type("string"):
            # "simple" message
            if part == "":
                part = "1"
            return self._parseMsgSimple(data, part)

        else:
            if part != "":
                part = part + "."

            # multipart message
            count = len(data)
            parts = [data[count-1]]  # the subtype (mixed, alternative etc)

            for i in range(0, count-1):  # should be >=2 anyway
                parts.append(self._parseMsg(data[i], "%s%d" % (part, i+1)))

            return parts


    def _fixEnvelope(self, envelope):
        """Structure the envelope data

        Used whenever a FETCH might've returned an ENVELOPE field"""

        # envelope is a tuple with the following elements, in this order:
        # DATE, SUBJECT, FROM, SENDER, REPLYTO, TO, CC, BCC, INREPLYTO, MSGID
        # This makes it a dictionary and puts the addresses in RFC822 format

        headers = ("DATE", "SUBJECT",
                    "FROM", "SENDER", "REPLYTO", "TO", "CC", "BCC",
                    "INREPLYTO", "MSGID")

        newenvelope = {}

        for i in range(0, len(headers)):
            try:
                list = envelope[i]

            except IndexError:
                list = None

            if list and type(list) == type([]):  # only headers are arrays
                newlist = []
                for addy in list:

                    # Header fields are tuples, each individual element is
                    # itself a tuple: ( NAME, ROUTE, USERNAME, HOST )
                    # Special case where HOST or USERNAME are None (grouping)
                    # is not treated and will probably break something
                    # Similarly, no escaping is attempted on address components

                    email = {'name': addy[0],
                             'email': '%s@%s' % (addy[2], addy[3])}
                    newlist.append(email)
                list = newlist

            newenvelope[headers[i]] = list

        return newenvelope


    def _parseFlags(self, flags):
        "Make a dictionary out of the ['\\flag1','\\flag2'] list"

        parsed = {}

        if flags:
            for flag in flags:
                if flag[0] == '\\':
                    flag = flag[1:]
                flag = upper(flag)
                parsed[flag] = 1

        return parsed


# Test code
if __name__ == '__main__':
    import os

    testdir = "/tmp/imap4rev1mbox"

    # Create test mail directory
    os.system("rm -rf %s" % testdir)
    os.system("maildirmake %s" % testdir)

    m = IMAP4(maildir=testdir)

    m.list(getSubscribed=0)

    # aah. later.


