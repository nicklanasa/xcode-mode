//
//  ViewController.swift
//  test-workspace
//
//  Created by Nickolas Lanasa on 1/29/16.
//  Copyright © 2016 Nickolas Lanasa. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    @IBAction func buttonTapped(sender: AnyObject) {
        NSLog("Hello, world")
    }

}

