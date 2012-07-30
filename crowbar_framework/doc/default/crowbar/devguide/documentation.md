### Adding Documentation

Crowbar uses a composite documentation system that allows each barclamp to add documentation specific to its function while still building a single comprehensive documentation set.

> _Tip:_ Use http://www.ctrlshift.net/project/markdowneditor/ to WYSIWYG edit markdown!

This information is available as a video! see http://youtu.be/eWHeEWiOEvo

#### Index
The core of this system is a documentation tree index for each barclamp.  The index is a yml file which builds the documentation tree by deep merging all the barclamp indexes together.

The index _must_ be named the same as the barclamp.  So each barclamp provides "barclamp.yml" in the `doc` directory.  These files are merged together during the barclamp install. 

The index has an entry for each topic page that follows the following pattern: `barclamp+topic`.  The plus is required!

It is acceptable for a barclamp to reference topics in another barclamp so that the correct parent topics are used to build an integrated set.

The index file should be nested so that topics have correct parents.

Each level of the index can have meta data overrides, the meta data values are:

* order - defaults to alpha if omitted.  Range is 0 to 999999, default is 9999
* author - who edited this document last
* license - the license the document is distributed under
* date - last edit date
* copyright - copyright requirements for the document
* url - link to part of Crowbar this information relates to

An example index file looks like this:

    # theses are the default meta_data values
    license: Apache 2
    copyright: 2012 by Dell, Inc
    author: Dell CloudEdge Team
    date: July 29, 2012
    crowbar+book-userguide:
      order: 1000
      crowbar+ui-general:
        order: 10
        - crowbar+ui-nodes
      crowbar+ui-utilities:
        order: 100
        url: '/utils'
        crowbar+utils-export:
          url: '/utils/index'
          

#### Topic Documentation

Markdown is the current format.

Path is `doc/default/barclamp/file.md`

It is allowable (recommended!) to use additional subdirectories!  Simply add another + to the same to indicate another level of depth on the file path.

The name of the topic is expected to start with a # title flag.  This title is used by the generator to create the displayed title.

It is recommended to use # for top level (book), ## for second level (section), ### for third level (topic) and so on.

