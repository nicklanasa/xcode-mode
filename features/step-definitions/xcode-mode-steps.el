(Then "^I should see a list of workspaces$"
  (lambda ()
		(xcode-find-workspaces-for-directory "~/Dropbox/Developer/iOS/MyReddit")))
