Feature: Running
  
  Scenario: Running a project
    When I open a file "examples/test-project/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x rr"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "BUILD SUCCEEDED"
    And I should not see "BUILD FAILED"
