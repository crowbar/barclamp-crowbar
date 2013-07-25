SimpleNavigation::Configuration.run do |navigation|
  navigation.items do |primary|
    primary.item :nodes, t('nav.nodes'), root_path do |secondary|
      secondary.item :dashboard, t('nav.dashboard'), dashboard_path()
      secondary.item :bulkedit, t('nav.list'), nodes_list_path(:allocated=>'yes')
      # insert here for :nodes
    end
    primary.item :network, t('nav.network'), network_path do |secondary|
      # insert here for :network
      secondary.item :switch, t('nav.switch'), switch_path #FROM BARCLAMP: network.
      secondary.item :vlan,   t('nav.vlan'),   vlan_path   #FROM BARCLAMP: network.
    end
    primary.item :barclamps, t('nav.barclamps'), barclamp_modules_path do |secondary|
      secondary.item :barclamps, t('nav.all_bc'), barclamp_modules_path
      secondary.item :crowbar, t('nav.crowbar_bc'), index_barclamp_path(:controller=>'crowbar')
      # insert here for :barclamps
      secondary.item :openstack, t('nav.openstack'), index_barclamp_path(:controller=>'openstack') #FROM BARCLAMP: openstack.
      # insert here for :add  (this is legacy support)
    end
    primary.item :utils, t('nav.utils'), utils_path do |secondary|
      secondary.item :util_index, t('nav.util_logs'), utils_path
      # insert here for :utils
      secondary.item :swift, t('nav.swift'), "/swift/dashboard/" #FROM BARCLAMP: swift.
    end
    primary.item :help, t('nav.help'), docs_path
  end
end
