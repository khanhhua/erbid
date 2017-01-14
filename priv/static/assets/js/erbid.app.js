/**
 * Created by khanhhua on 1/2/17.
 */
(function (erbidApp) {

  function DashboardComponentCtrl($scope, $http, $uibModal, erbidService) {
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
    };

    $scope.onPlaceBidClick = function (listing) {
      var modalInstance = $uibModal.open({
        component: 'erbidBiddingModal',
        resolve: {
          listing: listing
        }
      });

      modalInstance.result.then(function (result) {
        refreshListings();
      });
    };

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
  DashboardComponentCtrl.$inject = ['$scope','$http','$uibModal','erbidService'];
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

  erbidApp.component('erbidBiddingModal', {
    bindings: {
      resolve: '<',
      close: '&',
      dismiss: '&'
    },
    templateUrl: '/assets/partials/erbid-bidding-modal.html',
    controller: function ($scope, $http, erbidService) {
      $ctrl = this;
      $ctrl.$onInit = function () {
        $scope.listing = $ctrl.resolve.listing;
      }
      $scope.isBusy = false;

      $scope.onBidClick = function () {
        var listingId = $scope.listing.id;
        var bidValue = $scope.listing.price + 100;

        $scope.isBusy = true;

        erbidService.placeBid(listingId, bidValue).then(function (result) {
          if (result) {
            console.info('[onPlaceBidClick] Successfully placed a bid');
          }
          else {
            console.warn('[onPlaceBidClick] Could not place a bid');
          }
        }).finally(function () {
          $scope.isBusy = false;
        });
      };

      $scope.onCloseClick = function () {
        $ctrl.close();
      };
    }
  });

  erbidApp.service('erbidService', ['$http', function ($http) {
    // REPEAT MY MANTRA
    // - All function in this service returns promises
    // - Promises resolve

    return {
      placeBid: placeBid
    };

    /**
     *
     * @param listingId
     * @param bidValue
     * @return {Promise<boolean>}
     */
    function placeBid (listingId, bidValue) {
      return $http.post('/api/placeBid', {
        listing_id: listingId,
        bid_value: bidValue
      }).then(function (res) {
        return true;
      }).catch(function (err) {
        console.error(err);

        return false;
      });
    }
  }]);

  erbidApp.factory('erbid.httpInterceptor', ['$sessionStorage', function ($sessionStorage) {
    return {
      request: function (config) {
        if (config.url.indexOf('/api') === 0 && $sessionStorage.authtoken) {
          config.headers['Authorization'] = 'bearer ' + $sessionStorage.authtoken;
        }

        return config;
      }
    }
  }]);

  erbidApp.config([
    '$httpProvider',
    '$locationProvider',
    '$stateProvider',
    '$urlRouterProvider',
    function ($httpProvider, $locationProvider, $stateProvider, $urlRouterProvider) {
      $httpProvider.interceptors.push('erbid.httpInterceptor');

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
})(angular.module('erbid.app', ['ngStorage', 'ngResource','ngRoute','ui.router', 'ui.bootstrap.modal', 'ui.bootstrap.tpls']));
