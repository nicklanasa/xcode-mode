Feature: Cleaning
  
  Scenario: Cleaning a project
    When I open a file "examples/test-project/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x cc"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "CLEAN SUCCEEDED"
    And I should not see "CLEAN FAILED"
