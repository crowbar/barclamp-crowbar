SimpleNavigation::Configuration.run do |navigation|  
  navigation.items do |primary|
    primary.item :nodes, t('nav.nodes'), root_path do |secondary|
      secondary.item :dashboard, t('nav.dashboard'), root_path
      secondary.item :dashboard, t('nav.list'), nodes_list_path
    end
    primary.item :barclamps, t('nav.barclamps'), barclamp_modules_path do |secondary|
      # barclamps can add menu items here
    end
    primary.item :help, t('nav.help'), '/users_guide.pdf', { :link => { :target => "_blank" } }
  end
end
