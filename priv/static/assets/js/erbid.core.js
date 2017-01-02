(function (erbidCore) {
  function LoginCtrl($scope, $http, $sessionStorage, $window) {
    $scope.username = '';

    $scope.onLoginClick = function () {


      $http.post('/api/login',
        {
          username: $scope.username
        }
      ).then(function (res) {
        var data = res.data;

        if (data.ok === true) {
          $sessionStorage.authtoken = data.authtoken;
          $window.location.href = '/app';
        }
        else {
          alert('Cannot login');
        }
      })
    };
  }

  LoginCtrl.$inject = ['$scope', '$http', '$sessionStorage', '$window'];
  erbidCore.controller('LoginCtrl', LoginCtrl);


  erbidCore.component('erbidBidList', {
    templateUrl: '/assets/partials/erbid-bid-list.html',
    controller: BidListController
  });
  function BidListController($scope, erbidListings) {
    $scope.bids = [];

    this.$postLink = function () {
      this.getActiveBid().then(function (bids) {
        $scope.bids = bids;
      });
    };

    this.getActiveBid = function () {
      return erbidListings.get().$promise.then(function (resource) {
        return resource.listings;
      });
    };
  }

  BidListController.$inject = ['$scope', 'erbid.listings'];


  erbidCore.service('erbid.listings', ['$resource', function ($resource) {
    return $resource('/api/resources/listings');
  }]);
})(angular.module('erbid.core', ['ngResource', 'ngStorage']));
