SimpleNavigation::Configuration.run do |navigation|  
  navigation.items do |primary|
    primary.item :dashboard, t('nav.dashboard'), root_path
    if RAILS_ENV == 'development'
      primary.item :barclamps, t('nav.barclamps'), barclamp_modules_path do |secondary|
        # barclamps can add menu items here
        secondary.item :foo, "testing", '/users_guide.pdf'
      end
    else
      primary.item :barclamps, t('nav.barclamps'), barclamp_index_barclamp_path do |secondary|
        # barclamps can add menu items here
      end
      primary.item :proposals, t('nav.proposals'), barclamp_proposals_barclamp_path
      primary.item :roles, t('nav.roles'), barclamp_roles_barclamp_path
    end
    primary.item :help, t('nav.help'), '/users_guide.pdf', { :link => { :target => "_blank" } }
  end
end
