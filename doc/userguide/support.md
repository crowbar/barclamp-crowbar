## Support / Utilities Menu

### Bootstrap Wizard

We have a shiny installation that you can use to finish bootstrapping
your admin node.  

To use it, pass the =--wizard= flag after your FQDN to
/opt/dell/bin/install-crowbar when setting up the admin node, and the
install script will not automatically create an admin network or an
entry for the admin node, and logging into the web UI will let you
customize things before creating the initial admin node entry and
committing the system deployment.  Right now the wizard only works
with Firefox, we are debugging to see what is breaking with Chrome.

Once we get closer to releasing CB 2.0, --wizard will become the default.


### Barclamps

List of installed barclamps