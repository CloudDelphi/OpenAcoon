### About the sources for the Query-CGI:

You will need to change the end of PostProcess.pas. You will see a line there that says:

```pascal
cDefaultBaseDir := '..\..\ranking\';
```

You will need to change that path to where you put the "ranking" directory on your server.

The Query-CGI uses `query_template.html` as the template for the results. This file needs
to be in the same directory as the `query.exe`.
