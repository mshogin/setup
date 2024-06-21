class profile::nodejs {

  # $enhancers = [
  #   'nodejs'
  # ]
  # package { $enhancers: ensure => 'installed' }

  class { '::nodejs':
    repo_url_suffix => '14.x',
    nodejs_package_ensure => '14.17.*',
    npm_package_ensure        => 'absent'
  }
}
