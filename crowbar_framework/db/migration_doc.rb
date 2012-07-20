
class DocBase < ActiveRecord::Migration
  def self.up

    create_table "docs", :force => true do |t|
      t.column "handle", :string, :size=>120, 
      t.column "parent_handle", :string, :size=>120
      t.column "title", :string, :size=>120
      t.column "author", :string, :size=>80
      t.column "license", :string,  :size=>20
      t.column "copyright", :string,  :size=>60
      t.column "date", :size=>20
      t.column "order", :size=>5, :default => 'alpha'
      t.timestamps
    end
    
    Docs.add(:handle=>'root', :barclamp=>'crowbar', :title=>I18n.t('docs.root'), :author=>'System', :license=>'Apache 2', :date=>'July 17, 2012')

  end

  def self.down
    drop_table "docs" 
  end
end