SimpleNavigation::Configuration.run do |navigation|  
  navigation.items do |primary|
    primary.item :nodes, t('nav.nodes'), root_path do |secondary|
      secondary.item :dashboard, t('nav.dashboard'), dashboard_path()
      secondary.item :bulkedit, t('nav.list'), nodes_list_path(:allocated=>'yes') 
      if ENV['RAILS_ENV'] == 'development'
        secondary.item :classes, t('nav.classes'), nodes_class_path
      end
      # insert here for :nodes
    end
    primary.item :network, t('nav.network'), network_path do |secondary| 
      secondary.item :vlan, t('nav.vlan'), vlan_path
      # insert here for :network
    end
    primary.item :barclamps, t('nav.barclamps'), barclamp_modules_path do |secondary|
      secondary.item :barclamps, t('nav.all_bc'), barclamp_modules_path
      secondary.item :crowbar, t('nav.crowbar_bc'), index_barclamp_path(:controller=>'crowbar')
      secondary.item :openstack, t('nav.openstack'), index_barclamp_path(:controller=>'openstack')
      # insert here for :barclamps 
      # insert here for :add  (this is legacy support)
    end
    primary.item :utils, t('nav.utils'), utils_path do |secondary| 
      secondary.item :util_import, t('nav.util_import'), utils_import_path 
      secondary.item :util_index, t('nav.util_logs'), utils_path 
      secondary.item :util_chef, t('nav.util_chef'), export_chef_path 
      secondary.item :log_export, t('nav.log_export'), utils_barclamp_path(:controller=>'logging')
      # insert here for :utils
    end
    primary.item :help, t('nav.help'), '/crowbar_users_guide.pdf', { :link => { :target => "_blank" } } do |secondary|
      secondary.item :help, t('nav.crowbar_wiki'), 'https://github.com/dellcloudedge/crowbar/wiki/', { :link => { :target => "_blank" } }
      secondary.item :help, t('nav.crowbar_ug'), '/crowbar_users_guide.pdf', { :link => { :target => "_blank" } }
      secondary.item :help, t('nav.crowbar_dg'), '/crowbar_deployment_guide.pdf', { :link => { :target => "_blank" } }
      secondary.item :openstack, t('nav.openstack'), "/openstack_users_guide.pdf", { :link => { :target => "_blank" } }
      # insert here for :help 
    end
  end
end
