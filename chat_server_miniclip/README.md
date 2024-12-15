# chat_server_miniclip

# Description
**chat_server_miniclip** is an easy chat server developed in Erlang to communicate with a client.

# You'll need...
- **For project**: Erlang/OTP 27.
- **For build**: Rebar3.

# Build
- rebar3 compile

# Compile
-rebar3 release
_build/default/rel/chat_server_miniclip/bin/chat_server_miniclip console

# Connection
- **client:**rebar3 shell
- **server:** telnet localhost 8080

# Quitting
- Press: Ctrl+]
- Type: quit