/**
 * Created by khanhhua on 1/2/17.
 */
(function (erbidApp) {

  function DashboardComponentCtrl($scope, $http) {
    $scope.listings = [];

    $http.get('/api/resources/listings').then(function (res) {
      if (!res.data.listings) {
        return;
      }

      var listings = res.data.listings;
      $scope.listings = listings;
    });
  }
  DashboardComponentCtrl.$inject = ['$scope','$http'];
  erbidApp.component('erbidDashboard', {
    templateUrl: '/assets/partials/erbid-dashboard.html',
    controller: DashboardComponentCtrl
  });

  erbidApp.component('account', {
    templateUrl: '/assets/partials/erbid-account.html'
  });

  erbidApp.component('erbidListingModal', {
    templateUrl: '/assets/partials/erbid-listing-modal.html',
    controller: function ($scope, $http) {
      $scope.onPostClick = function () {
        $http.post('/api/resources/listings', {
          title:'Cat Truck',
          description:'Lorem ipsum',
          price: 30,
          deadline:'2017-01-01T07:00:00Z',
          image_url:'https://placeholdit.imgix.net/~text?txtsize=28&bg=ccc&txt=300%C3%97300&w=300&h=300'
        }).catch(function (res) {
          console.error(res);
        });
      };
    }
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
