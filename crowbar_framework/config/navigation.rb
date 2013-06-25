SimpleNavigation::Configuration.run do |navigation|  
  navigation.items do |primary|
    primary.item :nodes, t('nav.nodes'), root_path do |secondary|
      secondary.item :dashboard, t('nav.dashboard'), dashboard_path()
      secondary.item :bulkedit, t('nav.list'), nodes_list_path(:allocated=>'yes') 
      secondary.item :families, t('nav.families'), nodes_families_path if RAILS_ENV == 'development'
      # insert here for :nodes
    end
    primary.item :network, t('nav.network'), network_path do |secondary| 
      # insert here for :network
    end
    primary.item :barclamps, t('nav.barclamps'), barclamp_modules_path do |secondary|
      secondary.item :barclamps, t('nav.all_bc'), barclamp_modules_path
      secondary.item :crowbar, t('nav.crowbar_bc'), index_barclamp_path(:controller=>'crowbar')
      # insert here for :barclamps 
      # insert here for :add  (this is legacy support)
    end
    primary.item :utils, t('nav.utils'), utils_path do |secondary| 
      secondary.item :util_index, t('nav.util_logs'), utils_path 
      # insert here for :utils
    end
    primary.item :help, t('nav.help'), '/crowbar_users_guide.pdf', { :link => { :target => "_blank" } } do |secondary|
      secondary.item :help, t('nav.crowbar_wiki'), 'https://github.com/crowbar/crowbar/wiki/', { :link => { :target => "_blank" } }
      secondary.item :help, t('nav.crowbar_ug'), '/crowbar_users_guide.pdf', { :link => { :target => "_blank" } }
      secondary.item :help, t('nav.crowbar_dg'), '/crowbar_deployment_guide.pdf', { :link => { :target => "_blank" } }
      secondary.item :documentation, t('nav.documentation'), docs_path if RAILS_ENV == 'development'
      # insert here for :help 
    end
  end
end
