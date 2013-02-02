Feature: Barclamp Graph
  In order inspect the barclamps
  The developer, Diane
  wants inspect the Barclamp > Instance > Role > Attrib > Node graph

  Scenario: Barclamp Graph
    When I go to the "barclamp/graph" page
    Then I should see {bdd:crowbar.i18n.barclamp.graph.title}
      And I should see heading "crowbar \\(Self-referential barclamp enabling other barclamps\\)"
      And I should see {bdd:crowbar.i18n.barclamp.graph.parents}
      And I should see {bdd:crowbar.i18n.barclamp.graph.members}
      And I should see {bdd:crowbar.i18n.barclamp.graph.configs}
      And I should see {bdd:crowbar.i18n.barclamp.graph.template}
      And there should be no translation errors
