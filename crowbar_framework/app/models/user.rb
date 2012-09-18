#
# Copyright 2012, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
require 'digest/md5'

class User < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :token_authenticatable, :confirmable,
  # :lockable, :timeoutable and :omniauthable
  # JimC:  :recoverable, :rememberable, :trackable, :validatable,  :database_authenticatable, :registerable,
  devise :database_authenticatable, :registerable,
         :rememberable, :trackable, :validatable

  # Setup accessible (or protected) attributes for your model
  # attr_accessible :email, :password, :password_confirmation, :remember_me
  # Include default devise modules. Others available are:
  # :token_authenticatable, :confirmable,
  # :lockable, :timeoutable and :omniauthable
  # JimC: confirmable recoverable
  devise :database_authenticatable, :registerable,
         :rememberable, :trackable,
         :lockable, :timeoutable, :authentication_keys => [:username]

  # Setup accessible (or protected) attributes for your model
  attr_accessible :username, :is_admin, :email, :password, :password_confirmation, :remember_me, :encrypted_password
  # attr_accessible :title, :body

  validates :username, :uniqueness => {:case_sensitive => false}

  DIGEST_REALM = "Crowbar - By selecting OK are agreeing to the License Agreement"

  def digest_password(new_pass)
    self.encrypted_password = digester(new_pass)
  end
  
  def email_required?
    false
  end
 
  def valid_password?(password)
    # try default password test first DatabaseAuthenticatable valid_password?
    # see https://github.com/plataformatec/devise/blob/master/lib/devise/models/database_authenticatable.rb
    return false if encrypted_password.blank?
    begin
      bcrypt   = ::BCrypt::Password.new(encrypted_password)
      password = ::BCrypt::Engine.hash_secret("#{password}#{self.class.pepper}", bcrypt.salt)
      return true if Devise.secure_compare(password, encrypted_password)
    rescue
      # ignore, keep going
    end
    
    # now the compromise, fall back to MD5 emthod as backup
    return digester(password).eql?(encrypted_password)
  end
 
  private
 
  def digester(pass)
    Digest::MD5.hexdigest([username,DIGEST_REALM,pass].join(":"))
  end
 
end

