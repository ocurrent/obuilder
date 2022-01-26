; Tests a bug where some directories/file from previous steps that were
; deleted where still hanging around.

((from patricoferris/empty@sha256:b95ca2b377dd736f609f0a10d63318c9f627a817823cf416402a29bfe9202bb9)
 (run (shell "mkdir hello"))
 (run (shell "echo 'hello world' > ./hello/hello.txt"))
 (run (shell "cat ./hello/hello.txt"))
 (run (shell "rm -rf hello"))
 (run (shell "ls")))