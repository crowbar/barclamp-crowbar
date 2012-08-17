class UsersController < Devise::SessionsController

  def sign_out
    destroy
  end

  def index
    @users = User.all
  end

end

