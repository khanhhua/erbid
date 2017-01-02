/**
 * Created by khanhhua on 1/2/17.
 */
(function (erbidApp) {

  function DashboardComponentCtrl($scope) {
    $scope.userBids = [
      {
        title: 'Eier',
        description: 'Eggwhite for goofies',
        bidderCount: 40
      },
      {
        title: 'Eier',
        description: 'Eggwhite for goofies',
        bidderCount: 40
      },
      {
        title: 'Eier',
        description: 'Eggwhite for goofies',
        bidderCount: 40
      },
      {
        title: 'Eier',
        description: 'Eggwhite for goofies',
        bidderCount: 40
      },
      {
        title: 'Eier',
        description: 'Eggwhite for goofies',
        bidderCount: 40
      },
      {
        title: 'Eier',
        description: 'Eggwhite for goofies',
        bidderCount: 40
      }
    ];
  }
  DashboardComponentCtrl.$inject = ['$scope'];
  erbidApp.component('erbidDashboard', {
    templateUrl: '/assets/partials/erbid-dashboard.html',
    controller: DashboardComponentCtrl
  });

  erbidApp.component('account', {
    templateUrl: '/assets/partials/erbid-account.html'
  });

  erbidApp.config([
    '$locationProvider',
    '$stateProvider',
    '$urlRouterProvider',
    function ($locationProvider, $stateProvider, $urlRouterProvider) {
      $locationProvider.html5Mode(false);

      $stateProvider.state({
        name: 'dashboard',
        url: '/',
        component: 'erbidDashboard'
      });

      $stateProvider.state({
        name: 'account',
        url: '/account',
        component: 'account'
        // templateUrl: '/assets/partials/erbid-account.html'
      });

      $urlRouterProvider.otherwise('/');
    }]);
})(angular.module('erbid.app', ['ngResource','ngRoute','ui.router']));
