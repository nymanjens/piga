Task Keeper
===========

[![CI Status](https://github.com/nymanjens/piga/actions/workflows/ci.yml/badge.svg)](https://github.com/nymanjens/piga/actions)

This project aims to be a list editor for power users.

Example use cases:

* A TODO list
* A list of meals and ingredients
* Notes from a meeting
* A list of things to remember

## Features

* Document editing:
    * Productivity **shortcuts** for actions such as 'remove task', 'swap task down',
      'open link', 'convert selection to lower case', ...
    * Ability to **collapse** all indented tasks beneath selected task
    * Tasks can have **tags**
* Many documents:
    * Easy **switching** between documents

## Screenshot

![screenshot](screenshot.png "Screenshot")

## Installation from release

- Download "Binaries (compiled files) with demo configuration" from the [latest
  release](https://github.com/nymanjens/piga/releases)
- Unpack the archive
- Open `conf/application.conf` in the unpacked folder:
  - Configure a database. The easiest way is to set up a MariaDB server locally,
    create an empty database called `facto` and configure it as follows:

```
db.default.driver=com.mysql.jdbc.Driver
db.default.url="jdbc:mysql://localhost/piga?user=mysqluser&password=mysqlpassword"
db.default.slick.profile = "slick.jdbc.MySQLProfile$"
```

  - Choose secret values for `play.http.secret.key` and `app.setup.defaultPassword`

- Open a terminal in the unpacked folder
- Run following commands to get the app running:

    ```
    # Create database tables
    bin/server -DdropAndCreateNewDb

    # Create admin user
    bin/server -DcreateAdminUser

    # Run application
    bin/server
    ```

- Browse to http://<ip_address>:9000/app/useradministration (username: "admin", password: "changeme")

## Installation with Docker

The following commands will launch a new server alongside a database in Docker containers:

```
# Get the docker-compose.yml file
wget https://raw.githubusercontent.com/nymanjens/piga/master/docker-compose.yml

# Choose a unique random string here of sufficient length
export APPLICATION_SECRET="$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1)"

# Create an empty database with a single admin user
docker-compose run web bin/server -DdropAndCreateNewDb
docker-compose run web bin/server -DcreateAdminUser

# Bring up the server
docker-compose up
```

When done, browse to http://<ip_address>:9000/app/useradministration (username: "admin", password: "changeme")

## Configuration
- `conf/application.conf`:<br>
  Setup and configure a database here.
- Add users:<br>
  A default user is created with the `-DcreateAdminUser` flag (username: admin, password:
  changeme). This account can create new users.

## Shortcuts

- **Basics**
    - **Indentation**
        - `tab`: Increase current indentation
        - `shift + tab`: Decrease current indentation
    - **Formatting**
        - `ctrl + I`: Toggle 'italic'
        - `ctrl + B`: Toggle 'bold'
        - ``ctrl + `` `: Toggle 'code font'
        - `alt + shift + 5`: Toggle 'strikethrough'
        - `ctrl + \`: Reset formatting
    - **Undo/redo**
        - `ctrl + Z`: undo
        - `ctrl + Y`: redo
        - `ctrl + shift + Z`: redo
- **Special actions**
    - `ctrl + K`: Create or edit a link
    - `alt + shift + T`: Create or edit a tag
    - `ctrl + P`: Open "Go to file" dialog
    - `ctrl + plus`: Expand current task
    - `ctrl + minus`: Collapse current task
- **Power user shortcuts**
    - **Copying**
        - `ctrl + shift + C`: Copy selected task and its children
        - `ctrl + shift + X`: Cut selected task and its children
        - `alt + shift + M`: Copy selected task and its children as Markdown
    - **Tasks**
        - `alt + up`: Swap current task with the previous task
        - `alt + down`: Swap current task with the next task
        - `ctrl + shift + P`: Go to the parent task
        - `ctrl + D`: Delete current task
        - `ctrl + shift + B`: Duplicate current task
    - **Change casing**
        - `ctrl + alt + U`: Convert selection to uppercase
        - `ctrl + shift + U`: Convert selection to uppercase
        - `ctrl + alt + L`: Convert selection to lowercase
        - `ctrl + shift + L`: Convert selection to lowercase
        - `alt + shift + L`: Convert selection to CamelCase
        - `alt + shift + K`: Convert selection to snake_case
        - `alt + shift + H`: Convert selection to dash-case
    - **Selection**
        - `ctrl + enter`: Open selected link
        - `ctrl + M`: Select current word
        - `ctrl + shift + M`: Select current quoted sentence
        - `ctrl + J`: Select current task
        - `ctrl + G`: Find next occurrence of selected text
    - **Other**
        - `ctrl + Q`: Go to the last edit
        - `ctrl + shift + delete`: Delete the remainder of the line after the cursor
