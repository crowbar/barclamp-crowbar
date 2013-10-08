### Adding Documentation

Crowbar uses a composite documentation system that allows each barclamp to add documentation specific to its function while still building a single comprehensive documentation set.

> Please see the Formating subsection for tips on formatting markdown.

This information is available as a video! see http://youtu.be/eWHeEWiOEvo

#### Composite Documentation

It is vital to understand that the Crowbar documentation system is _composite documentation._  That means that the information is assembled from multiple barclamps on the fly. This is required because the Crowbar framework is really a collection of barclamps and each barclamp has its own capabilities and features.

The design of the documentation system allows each barclamp to contribute parts to the overall whole _and also_ allows parts to cross reference each other.

For example, each barclamp is expected to contribute "barclamp" and "license" information. These pages only refer to the individual barclamp's data; however, they are rolled up under the barclamp and license sections of the documentation. For Crowbar suite barclamps, they are further grouped under the master Crowbar set. That means that the Deployer license information depends on the Crowbar Meta information.

While this adds complexity for the documentation author, it greatly simplify the documentation reading experience for the user. It also allows developers to isolate documentation changes.

#### Table of Contents - Directory Tree Layout

By design, the table of contents generally follows the directory structure of the documentation. This is intentional because it simplifies composition.

Each subdirectory should be paired with a matching topic document that functions as the index for the items in the subdirectory.

For example,

    devguide.md
    devguide/
        api.md
        api/
            node.md
            group.md
        testing.md
        testing/

In the above example, the `devguide` topic layout out general information for the developer guide. The `api` and `testing` sections would be shown as sections of the Developer Guide. Individual API topics `node` and `group` are subsections of the API topic.

> The convention of having an the index topic name (`api.md`) match the subfolder (`api/`) makes it easier for Crowbar to assemble to table of contents without extra meta data.

If no hints (see index below) are given, Crowbar will automatically scan the subdirectory tree and build the table of contents based on the file path naming convention.

#### Index
There is a documentation tree index (`[barclamp].yml`) for each barclamp. The index is a yml file which builds the documentation tree by deep merging all the barclamp indexes together.

> The index _must_ be named the same as the barclamp. So each barclamp provides "barclamp.yml" in the `doc` directory. These files are merged together during the barclamp install.

The index has an entry for each topic page that follows the following pattern: `barclamp/topic`. The `/` is required!

> You can comment out a page from being automatically index by prefixing its name with `#`

It is acceptable for a barclamp to reference topics in another barclamp so that the correct parent topics are used to build an integrated set.

The index file should be nested so that topics have correct parents.

> It is strongly encouraged (but not required) to keep the index path the same as the file path.

##### Ordering

You can control the order of documents within a directory by prefixing the file with a number followed by an underscore.

For example, a file named `333_sample_order.md` would be ordered as 333.

> If you omit order, the system defaults to 9999.

