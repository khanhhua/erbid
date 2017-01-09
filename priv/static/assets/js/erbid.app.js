/**
 * Created by khanhhua on 1/2/17.
 */
(function (erbidApp) {

  function DashboardComponentCtrl($scope, $http, $uibModal) {
    $scope.listings = [];
    this.$onInit = function () {
      refreshListings();
    };

    $scope.onPostListingClick = function () {
      var modalInstance = $uibModal.open({
        component: 'erbidListingModal'
      });

      modalInstance.result.then(function (result) {
        refreshListings();
      });
    }

    function refreshListings () {
      $http.get('/api/resources/listings').then(function (res) {
        if (!res.data.listings) {
          return;
        }

        var listings = res.data.listings;
        $scope.listings = listings;
      });
    }
  }
  DashboardComponentCtrl.$inject = ['$scope','$http','$uibModal'];
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
      $scope.input = {};

      $scope.onPostClick = function () {
        $http.post('/api/resources/listings', $scope.input)
          .then(function (res) {
            if (res.data) {
              $scope.$parent.$close('OK');
            }
          })
          .catch(function (res) {
            console.error(res);
          });
      };

      $scope.onCloseClick = function () {
        $scope.$parent.$close();
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
})(angular.module('erbid.app', ['ngResource','ngRoute','ui.router', 'ui.bootstrap.modal', 'ui.bootstrap.tpls']));
