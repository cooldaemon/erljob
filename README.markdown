## Erljob
the Erljob is a job scheduler in Erlang.

Here's a quick example illustrating how to use erljob:

    erljob:start(),
    erljob:add_job(greeting, fun (_X) -> io:fwrite("Hi!"), ok end, ok, 1000, 2),
    erljob:stop()

### How to Use
    % cd /path/to
    % git clone git://github.com/cooldaemon/erljob.git
    % cd ./to/erljob
    % make app PROJECT=kvs
    % cd ../kvs
    % make && make ct

