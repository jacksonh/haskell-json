Haskell JSON Service
=========

What
----

This is a program which runs as a FastCGI service (but which could
easily be changed to another Haskell web server.) Basic CGI is not
supported as a persistent process is needed for session data.

Install
-------

Download and build:

    $ git clone git://github.com/chrisdone/haskell-json.git
    $ cd haskell-json
    $ cabal configure && cabal build

Then a haskell.json executable is produced.

Here is tryhaskell.org's configuration, for example:

    $HTTP["host"] =~ "tryhaskell" {
       server.document-root       = "/var/www/tryhaskell.org"   
       cgi.assign = ( ".php" => "/usr/bin/php-cgi" )		    
       url.rewrite-once = ( "(.+)\.(js|css)$" => "gzip.php?file=$1.$2")
       # haskell json evaluator
       fastcgi.server =  ("haskell.json" =>
           ("localhost" =>
               ("socket" => "/home/chris/haskell-json/httpd/haskell-json.sock",
                "bin-path" => "/home/chris/haskell-json/dist/build/haskell.json/haskell.json",
                "bin-copy-environment" => ( "PATH", "SHELL", "USER" ),
                "max-procs" => 4,
                "docroot" => "/home/chris/haskell-json/")))
    }
