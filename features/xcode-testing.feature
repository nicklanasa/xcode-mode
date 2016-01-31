Feature: Testing
  
  Scenario: Testing a project
    When I open a file "examples/test-project/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x tt"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "2 passed"

  Scenario: Testing a workspace
    When I open a file "examples/test-workspace/.xctool-args"
    And I turn on xcode-mode
    And I press "C-c C-x tt"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "2 passed"

