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
1. debug - general information used for debugging problems and common issues
1. info - useful data about normal operations
1. warn - actions that are not normal and may require investigation
1. depricate - helps find class that have been depricated
1. custom - you can define your own levels (e.g.: using your name, mascot or martian crater) as long as you remember to include it in the configuration list.

Users set the logging level by including the desired level in the configuration.  Levels must be added explicitly!  There is no assumption of inclusion: if you want `trace` and `warn` then you need to add both to the logging list.

> The legacy `debug` methods are depricated and should be avoided!

You can use the log information in two ways

1. modify the configuration file when running tests (applies to all tests)
1. use `bdd:debug(config, feature, scenario_id#, [log, level, list]).` to call a single scenario with custom logging.  If you omit the list then it will default to `[puts, debug, info, warn]`

> Log messages can be very verbose!  Generally, running just 1 scenario is enough information for debugging.