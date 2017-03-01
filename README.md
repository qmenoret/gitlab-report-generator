
# Gitlab Report Generator

## Description
This tool takes a issue list from gitlab, and allow to produce a markdown table with all these issues, and to apply different filters, an sorts to it. You can also select which columns will be displayed.

## Cli reference
This generator can be used either by passing a JSON to stdin, or a file path as argument. It just can't do much yet.

Using file path
```bash
./gitlab-report-generator-exe --input-file <pathtofile>
```

Using stdin
```bash
# This actually only work with a maximum of 100 issues
curl --header "PRIVATE-TOKEN: <yourgitlabAPItoken>" "https://<GITLAB_INSTANCE>/api/v3/projects/<GROUPNAME/USERNAME>%2F<PROJECTNAME>/issues?per_page=100" 2> /dev/null| iconv -t UTF-8 | ./gitlab-report-generator-exe
```

Arguments:
`--input-file <file_path>` open this file instead of using stdin
`--filters <filters>` comma separated list of filters
`--sort-keys <keys>` keys on which we should sort issues
`--columns <keys>` fields to use as table headers

### Sort
You can sort the output by providing the keys on which you want the output to be sorted. You can specify several keys, which will be used in order. Keys are provided as a comma separated list of strings.

Example:
* "assignee.username,title" -> Will sort by assignee, and display in alphabetical order.

### Filters
You can apply filters on the issue list using the argument `--filters`.
The default is "".

#### Predefined

* open
* closed
* assigned
* unassigned
* active
* inactive

#### Dynamic

The parser support "dynamic" filters, which are basics expressions on the value of a field. It uses the stringified value of the field to produce the result.
The defaut is "title".

Examples:
* "id>4"                    --> Return all issues where id >  4
* "id=4"                    --> Return all issues where id == 4
* "id<4"                    --> Return all issues where id <  4
* "id~4"                    --> Return all issues where id =! 4
* "id>4,id<10"              --> Return all issues where id >  4 and id < 10
* "#id>4"                   --> Return all issues where not (id >  4)
* "description~"            --> Return all issues with empty description
* "assignee.username=jbond" --> Return all issues assigned to jbond
* "milestone=Nothing"       --> Return all issues not linked to a milestone

### Table headers
To select which fields are displayed in the table, use the `--columns` argument. It is a comma-separated list of strings.
The default is "title,state".

Example:
* "title,assignee.username,milestone.due_date" --> Display 3 columns
