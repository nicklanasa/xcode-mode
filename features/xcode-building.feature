Feature: Building
  
  Scenario: Building a project
    When I open a file "examples/test-project/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x bb"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "BUILD SUCCEEDED"
    And I should not see "BUILD FAILED"

  Scenario: Building a workspace
    When I open a file "examples/test-workspace/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x bb"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "BUILD SUCCEEDED"
    And I should not see "BUILD FAILED"
