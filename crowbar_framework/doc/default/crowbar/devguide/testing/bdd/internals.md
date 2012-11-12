### BDD Internals

This section documents the internal working of the BDD code framework.  The audience for this section is for developers who are extended the BDD system and want to learn more about how it operates.

Information about creating tests and tests should be included in other parts of the guide.

#### Logging (bdd_utils:log)

The BDD system has a logging utility that is managed from the Configuration file.

The core logging call is `bdd_utils:log(Config, Level, Message, Data).`  Shortcuts have been created, but are not recommended because they do not leverage ability for users the change the logging levels!  The short cuts are: `log(Level, Message, Data)`, `log(Message, Data)`, and `log(Message)` where the assumed Level is `info`

The logging system offers several levels:

1. true - always show the message
1. puts - the lowest level for debugging info that should be removed and not left in the code
1. trace - the lowest level of in-code statement, very verbose but helps trace entry of routines
1. debug - general information used for debugging problems
1. info - useful data about normal operations
1. warn - actions that are not normal and may require investigation

Users set the logging level by including the desired level in the configuration.  Levels must be added explicitly!  There is no assumption of inclusion: if you want `trace` and `warn` then you need to add both to the logging list.

> The legacy `debug` methods are depricated and should be avoided!