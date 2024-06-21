File { backup => false }
node default {
  # include role::mshogin
  # include role::bcserver
  exec { 'buidlbc':
    path => ["/usr/bin", "/usr/sbin"],
    environment => [
      "GOCACHE=/home/mshogin/.cache/go-build",
      "GOMODCACHE=/home/mshogin/go/pkg/mod",
      "GOPATH=/home/mshogin/go",
    ],
    command => "make -C /home/mshogin/iow/ti/bidcast/bidcast/"

  }
}
