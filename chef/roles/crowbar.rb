
name "crowbar"
description "Crowbar role - Setups the rails app"
run_list(
         "recipe[utils]",
         "recipe[crowbar]"
)
default_attributes(
  :crowbar => { :admin_node => true },
  :rails => { :max_pool_size => 256, :environment => "production" },
)
override_attributes()

