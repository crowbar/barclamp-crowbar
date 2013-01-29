### Log Marker

The Utils area has a log marketing feature to help with remote troubleshooting.

This is a very simple action that users can take to inject a specific string or set of characters into the logs to assist with troubleshooting.  

For example, if a user finds an error then they can use this feature to mark the log and make it easier for support reviews to find exactly when the error occured.

To invoke the log marker, use `utils/marker/[your message here]` from the Crowbar URL.

You should see the message echoed back to the screen.

This will inject `MARK >>>>> [your message here] <<<<< KRAM` into the logs at the INFO level.

> The BDD system automatically marks the log when it starts a test, executes a step or finds an error.