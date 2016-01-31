Feature: Archiving
  
  Scenario: Archiving a project
    When I open a file "examples/test-project/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x aa"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "ARCHIVE SUCCEEDED"
    And I should not see "ARCHIVE FAILED"
