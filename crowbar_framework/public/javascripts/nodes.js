//
// This JavaScript allow get value of attribute from nodes by name and attribute path
//
// Example:
//   var public = node('admin').get('network/networks/public') -> {router: "192.168.122.1", netmask: "255.255.255.0", router_pref: 5, add_bridge: false, ...}
//   public.router -> "192.168.122.1"
//
//   var router = node('admin').get('network/networks/public/router') -> "192.168.122.1"
//   router -> "192.168.122.1"
//
function node(name) {
    return {
        get: function(path) {
            var result = null;
            $.ajax({
                dataType: "json",
                async: false,
                url: "/nodes/"+name+"/attribute/"+path,
                success: function (data){
                    result = data.value;
                }
            });
            return result;
        }
    }
};

