### Barclamp APIs

#### SAMPLE - REFERENCE ONLY

**Input:**

| URL | Options | Returns | Comments |
|:---:|:-------:|:-------:|:--------:|
| /barclamp/status/2.0 | none | All proposals | Used by Barclamp List | 
| /barclamp/status/2.0/[id] | id is the proposal ID. | Used by Proposal Views |

**Output:**


    {
      "i18n":{"unknown":"Unknown, requesting status...","ready":"Active"},
      "proposals":{"5":"ready","11":"ready" },
      "count":14,
      "error":""
    }

Details:

* Format - json
* i18n - ?

