# Using pgx on mirage

### Network setup

Assuming one is using linux. The following steps are needed to setup networking for the mirage unikernel.

```
ip tuntap add tap100 mode tap
ip addr add 10.0.0.1/24 dev tap100
ip link set dev tap100 up

echo 1 > /proc/sys/net/ipv4/ip_forward # enables IP forwarding

# assuming "eth0" is your default network interface where all the traffic goes to the Internet.
iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
iptables -A FORWARD -i eth0 -o tap100 -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i tap100 -o eth0 -j ACCEPT
```

### Building and running the unikernel

```
opam install mirage
mirage configure -t hvt # replace hvt with spt/unix/xen etc
make depends
make
solo5-hvt --net:service=tap100 -- pgx_unikernel.hvt --pgpassword <password> --pguser <postgres user> --pghost <hostname for the postgres database> --pgport <port that the database is running on> --pgdatabase <database name to use> # The --pgdatabase flag is optional, but make sure to create the database before trying to run the example
```
