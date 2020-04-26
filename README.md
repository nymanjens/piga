Task Keeper
===========

[![Build Status](https://travis-ci.org/nymanjens/piga.svg?branch=master)](https://travis-ci.org/nymanjens/piga)

This project aims to be a list editor for power users.

Example use cases:

* A TODO list
* A list of meals and ingredients
* Notes from a meeting

## Features

*(Note: Some are not ready yet)*

* Document editing:
    * Productivity **shortcuts** for actions such as 'remove task', 'swap task down',
      'open link', 'convert selection to lower case', ...
    * Ability to **collapse** all indented tasks beneath selected task
    * Tasks can have **tags**
    * Ability to **search** in all documents
    * Taks can be **postponed** until a later date
* Many documents:
    * Easy **switching** between documents
    * Ability to **archive** documents and easily **search for documents**
* Works offline (via PWA)
* Dedicated mobile version

## Screenshot

![screenshot](screenshot.png "Screenshot")

## Installation

- Download the [latest release](https://github.com/nymanjens/piga/releases)
- Unpack the archive and open a terminal in the unpacked folder
- Run following commands to get the app running:

    ```
    # Create database tables
    bin/server -DdropAndCreateNewDb
    rm RUNNING_PID

    # Create admin user
    bin/server -DcreateAdminUser
    rm RUNNING_PID

    # Run application
    bin/server
    ```

- Browse to http://localhost:9000

## Configuration
- `conf/application.conf`:<br>
  Setup and configure a database here.
- Add users:<br>
  A default user is created with the `-DcreateAdminUser` flag (username: admin, password:
  changeme). This account can create new users.
