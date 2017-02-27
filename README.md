
# How to use

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

### Available filters

* open
* closed
* assigned
* unassigned
* active
* inactive

