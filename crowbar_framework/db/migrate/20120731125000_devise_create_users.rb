class DeviseCreateUsers < ActiveRecord::Migration
  def change
    create_table(:users) do |t|
      ## Database authenticatable
      t.string :email,              :null => false, :default => ""
      t.string :encrypted_password, :null => false, :default => ""

      # crowbar additions
      t.string :username,           :null=>false, :default=>""
      t.boolean :is_admin,          :default=>false, :null=>false

      ## Recoverable
      t.string   :reset_password_token
      t.datetime :reset_password_sent_at

      ## Rememberable
      t.datetime :remember_created_at

      ## Trackable
      t.integer  :sign_in_count, :default => 0
      t.datetime :current_sign_in_at
      t.datetime :last_sign_in_at
      t.string   :current_sign_in_ip
      t.string   :last_sign_in_ip

      ## Confirmable
      t.string   :confirmation_token
      t.datetime :confirmed_at
      t.datetime :confirmation_sent_at
      t.string   :unconfirmed_email # Only if using reconfirmable

      ## Lockable
      t.integer  :failed_attempts, :default => 0 # Only if lock strategy is :failed_attempts
      t.string   :unlock_token # Only if unlock strategy is :email or :both
      t.datetime :locked_at

      ## Token authenticatable
      # t.string :authentication_token


      t.timestamps
    end
    
    add_index :users, :email
    add_index :users, :username, :unique => true
    add_index :users, :reset_password_token, :unique => true
    add_index :users, :confirmation_token,   :unique => true
    add_index :users, :unlock_token,         :unique => true
    # add_index :users, :authentication_token, :unique => true
  
    if Rails.env.development? or Rails.env.test?
      u = User.find_or_create_by_username!(:username=>'developer', :password=>'replace!me', :is_admin=>true)
      u.digest_password('Cr0wbar!')
      u.save!
    end

  end
  
end
