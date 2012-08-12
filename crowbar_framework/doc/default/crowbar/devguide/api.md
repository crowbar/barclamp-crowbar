## Crowbar API

This document is the reference guide for the Crowbar 2.0 API

### Using the API

The Crowbar API is RESTful and accepts/returns JSON.  While some arguments may return XML as an option, it is NOT a supported output.

The Crowbar API is versioned.  API urls include the Crowbar version of the API (e.g.: 1.0 or 2.0).  Please use the most highest version available!

> Legacy Note: some routes maintain a 1.0 API; however, these should all be considered deprecated!

### Node APIs

### Barclamp APIs

### Proposal APIs

#### Proposal Status

**Input:**

| URL | Options | Returns | Comments |
|:---:|:-------:|:-------:|:--------:|
| /proposal/status/2.0 | none | All proposals | Used by Barclamp List | 
| /proposal/status/2.0/[id] | id is the proposal ID. | Used by Proposal Views |

**Output:**


    {
      "i18n":{"unknown":"Unknown, requesting status...","ready":"Active"},
      "proposals":{"5":"ready","11":"ready" },
      "count":14,
      "error":""
    }

Details:

* Format - json
* i18n - the localized versions of the status strings for display.  Unknown is _always_ included
* proposals - the proposals (by database ID) requested in the API call with their current status
* count - the number of proposals returned, count < 0 indicates an error
* error - provides error information if the call returns an error
