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

class UserService < ServiceObject

  def create_user(user_attr)
    user = User.new()

    username = user_attr["username"]
    email = user_attr["email"]
    password = user_attr["password"]
    password_confirmation = user_attr["password_confirmation"]
    encrypted_password = user_attr["encrypted_password"]
    remember_me = user_attr["remember_me"]
    is_admin = user_attr["is_admin"]
    
    user.username = username
    user.email = email
    user.password = password
    user.password_confirmation = password_confirmation
    user.encrypted_password = encrypted_password
    user.remember_me = remember_me
    user.is_admin = is_admin
 
    user.save!
    user 
  end
 
  def user_get(id)
    user = nil
    begin
      user = User.find_by_id_or_username(id)
      [200,  user]
    rescue ActiveRecord::RecordNotFound => ex
      puts "ActiveRecord::RecordNotFound #{ex}"
      @logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      puts "RuntimeError #{ex}"
      [500, ex.message]
    end
  end
  
  def user_create(username, email, password, password_confirmation, remember_me=false, is_admin=false)
    @logger.debug("Entering service user_create #{username}, #{email}, #{password}, #{password_confirmation}, #{remember_me}, #{is_admin}")
    user = nil
    begin
      User.transaction do
        user = User.new(
            :username => username,
            :email => email,
            :password => password,
            :password_confirmation => password_confirmation,
            :remember_me => remember_me,
            :is_admin => is_admin)
        user.save!
      end
      [200, user] 
    rescue ActiveRecord::RecordNotFound, ActiveRecord::RecordInvalid, ArgumentError => ex
      @logger.warn(ex.message)
      [400, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end


  def user_update(id, username, email, remember_me=false, is_admin=false)
    @logger.debug("Entering service user_update #{id}")

    user = nil
    begin
      User.transaction do
        user = User.find_by_id_or_username(id)
        hash = {:username => username, :email => email, :remember_me => remember_me, :is_admin => is_admin }
        @logger.debug("Attribute hash is: #{hash.inspect}")
        user.update_attributes(hash)
        user.save!
      end
      [200, user]
    rescue ActiveRecord::RecordNotFound, ArgumentError => ex
      @logger.warn(ex.message)
      [400, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end


  def user_delete(id)
    @logger.debug("Entering service user_delete #{id}")
    begin
      user =  User.find_by_id_or_username(id)
      @logger.debug("Deleting user #{user.id}/\"#{user.username}\"")
      User.destroy(user)
      [200, ""]
    rescue ActiveRecord::RecordNotFound => ex
      @logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end
  
  def user_make_admin(id)
   user_update_admin id, true
  end
  
  def user_remove_admin(id)
   user_update_admin id, false
  end
  
  def user_update_admin(id_username, onOff=false)
    begin
      user = User.find_by_id_or_username(id_username)
      user.is_admin = onOff;
      user.save
      [200, user]
    rescue ActiveRecord::RecordNotFound => ex
      @logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end
  
  def user_lock(id)
     begin
      user = User.find_by_id_or_username(id)
      user.lock_access! unless (user.access_locked?)
      user.save
      [200, user]
    rescue ActiveRecord::RecordNotFound => ex
      @logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end
  
  def user_unlock(id)
     begin
      user = User.find_by_id_or_username(id)
      user.unlock_access! if (user.access_locked?)
      user.save
      [200, user]
    rescue ActiveRecord::RecordNotFound => ex
      @logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end

  def user_reset_password(id, password, password_confirmation)
    begin
      user = User.find_by_id_or_username(id)
      user.admin_reset_password = true 
      ret = user.reset_password!(password,password_confirmation)
      if ret
       return [200, user] 
      else
       raise RuntimeError, "Failed resetting password: #{password}, password confirmation: #{password_confirmation}"
      end
    rescue ActiveRecord::RecordNotFound => ex
      @logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      @logger.error(ex.message)
      [500, ex.message]
    end
  end
 
end
