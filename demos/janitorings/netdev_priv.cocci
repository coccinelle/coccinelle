@ rule1 @
type T;
struct net_device *dev;
@@

 dev = 
(
        alloc_netdev
| 
        alloc_etherdev
|
        alloc_fcdev
|
        alloc_fddidev
|
        alloc_hippi_dev
|
        alloc_trdev
|
        alloc_ltalkdev
|
        alloc_irdadev
| 
        alloc_etherdev_mq
)
   (sizeof(T), ...)

// if don't have the iso sizeof(T) => sizeof(E)
// dev = alloc_netdev(sizeof(*x), ...)

//>>> - alloc_irlandev
// but dont pass the sizeof, so cant get the type T.

//> alloc_orinocodev


@ rule1bis @
struct net_device *dev;
expression E;
@@
 dev->priv = E
//+ DANGER


@ rule2 depends on rule1 && !rule1bis  @
struct net_device *dev;
type rule1.T;
@@

- (T*) dev->priv
+ netdev_priv(dev)


// the iso drop_cast is not enough because T* in the previous rule
// can not be dropped. It's not a pure T.
@ rule3 depends on rule1 && !rule1bis @
struct net_device *dev;
@@

- dev->priv
+ netdev_priv(dev)
