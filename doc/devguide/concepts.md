# Crowbar Concepts 

## The operations challenge

A deployment framework is key to solving the problems of deploying,
configuring, and scaling open source clusters for cloud computing.

Deploying an open source cloud can be a complex undertaking. If you’re
using manual processes, you could spend days or even weeks working to
get your cloud up and running. And even then, your cloud never sits
still—it’s always on an upgrade or improvement path. You need to
continually deploy new servers, add management capabilities, and track
the upstream releases, while keeping the cloud running, and providing
reliable services to end users.

These were among the challenges that drove the development of the Crowbar
software framework from it's roots as an [OpenStack](http://OpenStack.org)
installer into a much broader orchestration tool.  Because of this evolution,
Crowbar has a number of architectural features to address these
challenges:

* Abstraction Around Orchestration

    Crowbar is designed to simplify the operations of large scale cloud
    infrastructure by providing a higher level abstraction on top of
    existing configuration management and orchestration tools based on
    a layered deployment model.

* Web Architecture

    Crowbar is implemented as a web application server, with a full user
    interface and a REST API.

* Platform Agnostic 

    Crowbar is designed to be platform and operating system agnostic.
    It supports discovery and provisioning from a bare metal state,
    including hardware configuration, updating and configuring BIOS and
    BMC boards, and operating system installation. Multiple operating
    systems and heterogenous operating systems are supported. 

* Modular Architecture

    Crowbar is designed around modular plugins called BarClamps, which
    allow for extensibility and customization while encapsulating 
    layers of deployment in manageable units.

* State Management Engine

    The core of Crowbar is based on a state machine that tracks nodes,
    roles, and their relationships in groups called deployments. The
    state machine is responsible for analyzing dependencies and scheduling
    state transitions.

* Data model 

    Crowbar uses a dedicated database to track system state and data.
    As discovery and deployment progresses, system data is collected
    and made available to other components in the system.  Individual
    components can access and update this data, reducing dependencies
    through a combination of deferred binding and runtime attribute
    injection.

* Network Abstraction

    Crowbar is designed to support a flexible network abstraction,
    where pyhsical interfaces, BMC's, VLANS, binding, teaming,and other low level
    features are mapped to logical conduits, which can be referenced by other
    components.  Networking configurations can be created dynamically to adapt
    to changing infrastructure. 

* Testing and Simulation 

    The Crowbar framework includes an integrated testing and simulation
    system that simplified development and testing of the core framework and 
    plug in components.


