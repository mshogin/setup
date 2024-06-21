class profile::common {
  include 'docker'
  class {'docker::compose':
    ensure => present
  }

  $enhancers = [
    'locate',
    'htop',
    'pipenv',
    'direnv',
  ]
  package { $enhancers: ensure => 'installed' }

  package { [
    'googleauth',
    'google-api-client',
  ]:
    ensure   => present,
    provider => puppet_gem
  }
}
