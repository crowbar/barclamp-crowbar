Feature: Barclamp Graph
  In order inspect the barclamps
  The developer, Diane
  wants inspect the Barclamp > Instance > Role > Attrib > Node graph

  Scenario: Barclamp Graph
    When I go to the "barclamp/graph" page
    Then I should see {bdd:crowbar.i18n.barclamp.graph.title}
      And I should see a heading "crowbar (Self-referential barclamp enabling other barclamps)"
      And I should see "Parents"
      And I should see "Members"
      And I should see "Configurations"
      And I should see "Template"
      And there should be no translation errors
