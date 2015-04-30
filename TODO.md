 * add more protocols
 * pull in scalacheck, make arbitrary instances for Message
 * do round trip testing on all protocol translation round trip serialization
 * figure out how to use scalaz-stream with a socket
 * probably do some round trip testing on sockets, too.
 

 Client:
  * establish connection with engine
  * decide if we need a handshake, or if this version is hardcoded...


 Engine:
  * everything
  * could the engine have a server socket, and then clients can just connect as they please?
    if so, we would need a handshake to say at least what protocol they are using. 
    i like this. no more hard coding clients.
  * should we have a way to add exchanges on the fly? maybe that can just be hardcoded for now

 Exchange:
  * everthing
  * 



