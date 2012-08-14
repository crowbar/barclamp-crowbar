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
