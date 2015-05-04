# Engines

To reproduce weird scalaz-stream tcp behavior:

In first terminal:

    ./sbt
    > runMain com.joshcough.engines.PrintSocket

In second terminal:

    ./sbt
    > runMain com.joshcough.engines.SenderSocket

Observe that nothing happens. I expected to see messages printed out. 
