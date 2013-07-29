namespace :crowbar do

  desc "Run schema migration on proposals"
  task :schema_migrate, [:barclamps] => :environment do |t, args|
    args.with_defaults(:barclamps => 'all')
    barclamps = args[:barclamps].split(' ')

    require 'schema_migration'

    if barclamps.include?('all')
        SchemaMigration.run
    else
      barclamps.each do |barclamp|
        SchemaMigration.run_for_bc barclamp
      end
    end
  end

  desc "Run schema migration on proposals for production environment"
  task :schema_migrate_prod, [:barclamps] do |t, args|
    RAILS_ENV = 'production'
    Rake::Task['crowbar:schema_migrate'].invoke(args[:barclamps])
  end

end
