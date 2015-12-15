Feature: Building
  
  Scenario: Building a workspace
    And I turn on xcode-mode
    And I press "C-c C-x bw"
    And I switch to buffer "*compilation-mode*"
