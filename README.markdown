## tcp_server
A simple module for implementing concurrent TCP servers in Erlang.

* Makes it easy to implement concurrent TCP servers,
* Provides typical TCP server behaviours, listen, accept, and so forth,
* Handles multiple requests concurrently by maintaining process pool,
* Supports active as well as passive mode of gen_tcp,
* Based on OTP principles.

### How to Use
    % cd /path/to
    % git clone git://github.com/cooldaemon/tcp_server.git
    % cd ./to/tcp_server
    % make app PROJECT=echo_server
    % cd ../echo_server
    % make && make ct

