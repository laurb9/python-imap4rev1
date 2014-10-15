#-----------------------------------------------------------------------------
# Copyright (C)2001-2002 InfoStreet, Inc.
#                        Laurentiu Badea lc@infostreet.com
#
# Created On:     Feb 19th, 2001
#-----------------------------------------------------------------------------
from __main__ import sys

"""
imaplib.IMAP4 extension class which can talk to a local imapd over a pipe
in addition to remotely via a socket.

The local connection is also preauthenticated so the initial login handshake
is skipped. This is specific to courier-imap but should work with any other
IMAP server that accepts the mailbox path on the command line and talks over
stdin/stdout
"""

import sys
import imaplib, re, random, string
from popen2 import popen2
from os import environ
from time import time

# This should be in a config file.
IMAPD = '/usr/bin/courier-imapd 2>/dev/null'

# What capabilities to advertize
IMAP_CAPABILITY = "IMAP4rev1 CHILDREN NAMESPACE THREAD=ORDEREDSUBJECT SORT"

DEBUG_COMMANDS = 1  # display commands sent to local imap server

# Add extensions to command list.
CommandsExtensions = {
    'SORT': ('SELECTED',)
    }

for (extension, state) in CommandsExtensions.items():
    imaplib.Commands[extension] = state


class SocketPipe:
    """
    Private class that 'emulates' the basic functions of a socket
    utilizing a pipe open to a courier-imapd process.
    NOTE: This only emulates the socket methods used by imaplib.
    """

    def __init__(self, command):
        # pipe[0] for read, pipe[1] for write
        # set buffer to 1 byte
        # (disables buffering because it confuses readline)
        environ["IMAP_USELOCKS"] = "0"
        self.pipe = popen2(command, 1)


    def send(self, data):
        if DEBUG_COMMANDS and len(data) < 400:
            print >>sys.stderr, data

        self.pipe[1].write(data)


    def recv(self, bufsize=0):
        # This is not used in imaplib but I implemented it for the
        # sake of symmetry.
        if (bufsize > 0):
            return self.pipe[0].read(bufsize)

        else:
            return self.pipe[0].read()


    def makefile(self, type='r'):
        "Make and return a file object"
        if type == 'r':
            return self.pipe[0]

        else:
            return self.pipe[1]


    def close(self):
        self.pipe[0].close()
        self.pipe[1].close()


    def __del__(self):
        self.close()


class IMAP4(imaplib.IMAP4):
    """
    IMAP4 extension class which adds the ability to talk to a local imapd
    """

    def __init__(self, host=None, port=imaplib.IMAP4_PORT, maildir=None):
        """
        modified imaplib.IMAP4.__init__()

        You can call it two ways:
        1) w/ host,port => remote server's settings (requires login())
        2) w/ maildir   => the path to user's Maildir (the 'INBOX')
        """

        # The rest of the method is the original __init__ method
        # Modified/added blocks are marked with ### INFOSTREET comments
        # except where we needed to just add the imaplib. prefix

        self.host = host
        self.port = port
        self.debug = imaplib.Debug
        self.state = "LOGOUT"
        self.literal = None  # A literal argument to a command
        self.tagged_commands = {}  # Tagged commands awaiting response
        self.untagged_responses = {}  # {typ: [data, ...], ...}
        self.continuation_response = ''  # Last continuation response
        self.tagnum = 0

        # ## INFOSTREET {
        if maildir:
            self.maildir = maildir
            # Start local imap server on pipe

            environ["IMAP_CAPABILITY"] = IMAP_CAPABILITY
            # could also set MAILDIRQUOTA  (courier-specific quota settings)
            self.openPipe()
        else:
            # Open socket to server

            self.open(host, port)
        # ## INFOSTREET }

        # Create unique tag for this session,
        # and compile tagged response matcher.

        self.tagpre = imaplib.Int2AP(random.randint(0, 31999))
        self.tagre = re.compile(r'(?P<tag>'
                + self.tagpre
                + r'\d+) (?P<type>[A-Z]+) (?P<data>.*)')

        # Get server welcome message,
        # request and store CAPABILITY response.

        # ## INFOSTREET {
        if __debug__ and self.debug >= 1:
            _mesg('new IMAP4 session, tag=%s' % self.tagpre)
        # ## INFOSTREET }

        self.welcome = self._get_response()
        if self.untagged_responses.has_key('PREAUTH'):
            self.state = 'AUTH'
        elif self.untagged_responses.has_key('OK'):
            self.state = 'NONAUTH'
#        elif self.untagged_responses.has_key('BYE'):
        else:
            raise self.error(self.welcome)

        cap = 'CAPABILITY'
        self._simple_command(cap)
        if not self.untagged_responses.has_key(cap):
            raise self.error('no CAPABILITY response from server')
        self.capabilities = tuple(string.split(string.upper(self.untagged_responses[cap][-1])))

        if __debug__ and self.debug >= 3:
            _mesg('CAPABILITIES: %s' % repr(self.capabilities))

        for version in imaplib.AllowedVersions:
            if not version in self.capabilities:
                continue
            self.PROTOCOL_VERSION = version
            return

        raise self.error('server not IMAP4 compliant')


    def openPipe(self):

        "Replacement for open() that uses SocketPipe instead of socket"

        self.sock = SocketPipe(IMAPD + " " + self.maildir)
        self.file = self.sock.makefile('r')


    def sort(self, criteria="(ALL)", sort="(DATE)",
             charset="US-ASCII"):
        """
        Implement the SORT IMAP extension (uses UID)

        This should be used if 'SORT' in self.capabilities
        Always returns UIDs
        Sort options: [REVERSE] ARRIVAL CC DATE FROM SIZE SUBJECT TO
        criteria are just like for SEARCH.
        """

        typ, dat = apply(self._simple_command, ("UID", "SORT",
                                                sort,
                                                charset,
                                                criteria))
        return self._untagged_response(typ, dat, "SORT")
