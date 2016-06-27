Feature: Building

  Scenario: Builds a project
    When I open a file "examples/test-project/.xctool-args"
    And I turn on xcode-mode
    And I press "C-M-x bb"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "BUILD SUCCEEDED"
    And I should not see "BUILD FAILED"

  Scenario: Builds a workspace
    When I open a file "examples/test-workspace/.xctool-args"
    And I turn on xcode-mode
    And I press "C-M-x bb"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "BUILD SUCCEEDED"
    And I should not see "BUILD FAILED"

Feature: Testing

  Scenario: Builds workspace tests
    When I open a file "examples/test-workspace/.xctool-args"
    And I turn on xcode-mode
    And I press "C-M-x bt"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "BUILD-TESTS SUCCEEDED"

Feature: Cleaning

  Scenario: Cleans workspace
    When I open a file "examples/test-workspace/.xctool-args"
    And I turn on xcode-mode
    And I press "C-M-x cc"
    And I switch to buffer "*compilation*"
    And I wait for compilation to finish
    Then I should see "CLEAN SUCCEEDED"
