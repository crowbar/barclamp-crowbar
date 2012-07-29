# Crowbar 2.x Developers Guide

Welcome to the amazing fuzziness of Crowbar!  This guide is targeted at people who want to extend Crowbar by adding new barclamps (aka modules). 

## Introduction
This document is a work in process!  Please update and then remove this tag when you feel that it’s complete!

### Caution!

This guide is NOT designed for Crowbar 1.x.

### Contributors:

* Rob Hirschfeld

### Prerequisites

Before you start developing a barclamp, we recommend that you do the following:

*	Learn how to use Crowbar
*	Write down the manual steps needed to install your software
*	Create Chef Cookbooks to deploy your software


## Making a Barclamp work with Crowbar

### Pending Sections?

* Creating UI screens
* Adding Chef cookbooks
* more?

### Adding Documentation

Crowbar uses a composite documentation system that allows each barclamp to add documentation specific to its function while still building a single comprehensive documentation set.

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

The name of the topic is expected to start with a # title flag.  This title is used by the generator to create the displayed title.

It is recommended to use # for top level (book), ## for second level (section), ### for third level (topic) and so on.

### Adding Localizations (i18n)

Crowbar uses Rails I18N library.  Please refer to the documentation (http://http://guides.rubyonrails.org/i18n.html) for usage hints that can help you reduce coding and add nifty features like Interpolation.

Each barclamp is expected to add its own localization (i18n) file.  Please do _not_ add your localizations into another barclamps i18n file, it’s not friendly!  However, you also need to be careful not to create duplicate entries.  That’s just too confusing for Crowbar and makes the bunny angry.

Add your localization file (`en.yml` is the default) into the `crowbar_framework/config/locales/[barclamp]` directory.  You know this but I’ve got to tell the n00bs: you need to replace [barclamp] with the name of your barclamp.

If you are supporting multiple languages, replace `en` with the target language code.  Like `kl.yml` if you want provide Klingon translations.

Inside the i18n file, you’ll provide a simple YML hash for translations.
    en:
      # Layout
      nav:
        nodes: Nodes
        nodes_description: Infrastructure Components

Reminder: encode your translations in quotes if you need to use : or ‘ marks!

#### Crowbar 1.0 note
We no longer support storing localization strings in the crowbar.yml meta data file.  This was not scaling so we dropped it like a rotten tomato in the Heinz ketchup factory. 

### Adding Menu Items

You add menu items into Crowbar using database migrations that insert into the `navs` table using the `Nav` object.

You must add the migration to the `crowbar_framework/db/migrate` directory and follow the Rails migration naming convention of `YYYYMMDDHHMMSS_barclamp_navs.rb`.

Inside the migration, use the `Nav.find_or_create_by_item` to populate the information for the menu item:

*	item = the id of the item
*	parent_item = the id of the top level menu you want to use (`root` creates a top level menu)
*	name = the i18n path to the menu text
*	description = the i18n path to the menu hover information
*	path = the Rails path you want to follow.  Unless it starts with http, `eval` will be applied to the path.
*	order = the display order of the menu item

Remember:

*	to provide a `self.down` that removes your menu item!  It’s just more sanitary that way.
*	to create matching entries in your barclamps i18n files

#### Example from the Network barclamp:
    class NetworkNavs < ActiveRecord::Migration
      def self.up
        Nav.find_or_create_by_item :item=>'switches', :parent_item=>'network', :name=>'nav.switch', :description=>'nav.switch_description', :path=>"switch_path", :order=>500
      end
    
      def self.down
        Nav.delete_by_item 'switches'
        Nav.delete_by_item 'vlan'
      end
    end

#### Crowbar 1.0 note

We no longer support storing navigation strings in the crowbar.yml meta data file.  This approach made difficult to upgrade and maintain.  The new approach is also difficult to maintain but easier to upgrade.
