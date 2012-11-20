class UsersController < BarclampController
  respond_to :html, :xml, :json
  
  def initialize # HACK HACK HACK HACK, act like a barclamp
    @bc_name = "user"
    @barclamp_object = Barclamp.new
    @barclamp_object.name = "user"
    super
  end
  
  def index
    setup_users
  end

  def unlock_user
    user = User.find(params[:id])
    user.unlock_access! if (user.access_locked?)
    redirect_to users_path
  end

  def lock_user
    user = User.find(params[:id])
    user.lock_access! if !(user.access_locked?)
    redirect_to users_path
  end

  def reset_password
    check_password
    
    @user = User.find(params[:user][:id])
    @user.admin_reset_password = true
    if @user.reset_password!(params[:user][:password],params[:user][:password_confirmation])
      redirect_to users_path, :notice => t("user.reset_password_success")
    else
      setup_users
      render :action => :index
    end
  end

  def edit
    @user = User.find(params[:id])
    edit_common
  end

  def edit_password
    @user = User.find(params[:id])
    @user.admin_reset_password = true
    edit_common
  end

  def edit_common
    setup_users
    render :action => :index
  end

  def update
    check_password

    @user = User.find(params[:id])
    if @user.update_attributes(params[:user])
      redirect_to users_path, :notice => t("user.update_success")
    else
      setup_users
      render :action => :index
    end
  end

  def create
    check_password
    
    @user = User.new(params[:user])
    if @user.save
      redirect_to users_path, :notice => t("user.create_success")
    else
      setup_users
      render :action => :index
    end
  end

  def delete_users
    if (params['users_to_delete'])
      User.destroy(params['users_to_delete'])
      notice = t("user.delete_success")
    else
      notice = t("user.none_selected")
    end
    redirect_to users_path, :notice => notice
  end
  
  ############################## API Calls ####################################
  
  add_help(:user_list,[],[:get])
  def users
    Rails.logger.debug("Listing users");

    user_refs = []

    User.all.each { |user|
      user_refs << user.id
    }

    respond_to do |format|
      format.json { render :json => user_refs }
      format.xml { render :xml => user_refs }
    end
  end

  add_help(:user_show,[:id],[:get])
  def user_show
    id = params[:id]

    Rails.logger.debug("Showing user #{id}");

    ret = operations.user_get(id)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_create,[:username, :email, :password, :password_confirmation, :remember_me, :is_admin],[:post])
  def user_create
    username = params[:username]
    email = params[:email]
    password = params[:password]
    password_confirmation = params[:password_confirmation]
    remember_me = params[:remember_me]
    is_admin = params[:is_admin]
    Rails.logger.debug("Creating user #{username}");
    ret = operations.user_create(username, email, password, password_confirmation, remember_me, is_admin)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    respond_to do |format|
      format.json { render :json => ret[1].to_json() }
    end
  end

  add_help(:user_update,[:id, :username, :email,:remember_me, :is_admin],[:put])
  def user_update
    id = params[:id]
    username = params[:username]
    email = params[:email]
    remember_me = params[:remember_me]
    is_admin = params[:is_admin]

    Rails.logger.debug("Updating user #{id}");

    ret = operations.user_update(id, username, email, remember_me, is_admin)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json() }
    end
  end

  add_help(:user_delete,[:id],[:delete])
  def user_delete
    Rails.logger.debug("Deleting user #{params[:id]}");

    ret = operations.user_delete(params[:id])
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:user_make_admin,[:id],[:get])
  def user_make_admin
    id = params[:id]

    Rails.logger.debug("Making user #{id} admin");

    ret = operations.user_make_admin(id)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_remove_admin,[:id],[:get])
  def user_remove_admin
    id = params[:id]

    Rails.logger.debug("Removing admin privilege for user #{id}");

    ret = operations.user_remove_admin(id)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_lock,[:id],[:get])
  def user_lock
    id = params[:id]

    Rails.logger.debug("Locking user #{id}");

    ret = operations.user_lock(id)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_unlock,[:id],[:get])
  def user_unlock
    id = params[:id]

    Rails.logger.debug("Unlocking user #{id}");

    ret = operations.user_unlock(id)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_reset_password,[:id, :password, :password_confirmation],[:put])
  def user_reset_password
    id = params[:id]
    password = params[:password]
    password_confirmation = params[:password_confirmation]

    Rails.logger.debug("Unlocking user #{id}");

    ret = operations.user_reset_password(id, password, password_confirmation)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  private

  def check_password
    if params[:user][:password].blank?
      params[:user].delete(:password)
      params[:user].delete(:password_confirmation)
    end
  end

  def setup_users
    @users = User.all
    @user ||= User.new
  end

end