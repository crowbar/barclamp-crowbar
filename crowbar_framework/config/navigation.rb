SimpleNavigation::Configuration.run do |navigation|  
  navigation.items do |primary|
    primary.item :nodes, t('nav.nodes'), root_path do |secondary|
      secondary.item :dashboard, t('nav.dashboard'), root_path if RAILS_ENV == 'development'
      secondary.item :dashboard, t('nav.list'), nodes_list_path if RAILS_ENV == 'development'
    end
    primary.item :barclamps, t('nav.barclamps'), barclamp_modules_path do |secondary|
      secondary.item :barclamps, t('nav.all_bc'), barclamp_modules_path if RAILS_ENV == 'development'
      # barclamps can add menu items here
    end
    primary.item :help, t('nav.help'), '/users_guide.pdf', { :link => { :target => "_blank" } }
  end
end
