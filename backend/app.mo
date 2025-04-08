// Real Estate Management System
import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Option "mo:base/Option";
import Hash "mo:base/Hash";

actor RealEstateManager {

  // Type definitions
  type PropertyId = Nat;
  type UserId = Principal;

  type Property = {
    id : PropertyId;
    owner : UserId;
    address : Text;
    description : Text;
    price : Nat;
    isForSale : Bool;
    isForRent : Bool;
    images : [Text]; // URLs to property images
    createdAt : Time.Time;
    updatedAt : Time.Time;
  };

  type PropertyListing = {
    property : Property;
    listingType : {#Sale; #Rent};
    price : Nat;
    rentalPeriod : ?{#Daily; #Weekly; #Monthly; #Yearly};
    listedAt : Time.Time;
    featuredUntil : ?Time.Time;
  };

  // State variables
  private stable var nextPropertyId : Nat = 1;
  private stable var entries : [(PropertyId, Property)] = [];
  private stable var userPropertyEntries : [(UserId, [PropertyId])] = [];
  private stable var listingEntries : [(PropertyId, PropertyListing)] = [];

  private var properties = HashMap.HashMap<PropertyId, Property>(0, Nat.equal, Hash.hash);
  private var userProperties = HashMap.HashMap<UserId, [PropertyId]>(0, Principal.equal, Principal.hash);
  private var listings = HashMap.HashMap<PropertyId, PropertyListing>(0, Nat.equal, Hash.hash);

  // Add these system functions to handle upgrades
  system func preupgrade() {
    entries := Iter.toArray(properties.entries());
    userPropertyEntries := Iter.toArray(userProperties.entries());
    listingEntries := Iter.toArray(listings.entries());
  };

  system func postupgrade() {
    properties := HashMap.fromIter<PropertyId, Property>(entries.vals(), entries.size(), Nat.equal, Hash.hash);
    userProperties := HashMap.fromIter<UserId, [PropertyId]>(userPropertyEntries.vals(), userPropertyEntries.size(), Principal.equal, Principal.hash);
    listings := HashMap.fromIter<PropertyId, PropertyListing>(listingEntries.vals(), listingEntries.size(), Nat.equal, Hash.hash);
    entries := [];
    userPropertyEntries := [];
    listingEntries := [];
  };

  // Create a new property
  public shared(msg) func createProperty(
    address: Text,
    description: Text,
    price: Nat,
    isForSale: Bool,
    isForRent: Bool,
    images: [Text]
  ) : async PropertyId {
    let caller = msg.caller;

    let propertyId = nextPropertyId;
    nextPropertyId += 1;

    let now = Time.now();

    let newProperty : Property = {
      id = propertyId;
      owner = caller;
      address = address;
      description = description;
      price = price;
      isForSale = isForSale;
      isForRent = isForRent;
      images = images;
      createdAt = now;
      updatedAt = now;
    };

    properties.put(propertyId, newProperty);

    // Update user's properties
    switch (userProperties.get(caller)) {
      case (null) {
        userProperties.put(caller, [propertyId]);
      };
      case (?existingProperties) {
        userProperties.put(caller, Array.append(existingProperties, [propertyId]));
      };
    };

    // If property is for sale or rent, create a listing
    if (isForSale) {
      let listing : PropertyListing = {
        property = newProperty;
        listingType = #Sale;
        price = price;
        rentalPeriod = null;
        listedAt = now;
        featuredUntil = null;
      };
      listings.put(propertyId, listing);
    } else if (isForRent) {
      let listing : PropertyListing = {
        property = newProperty;
        listingType = #Rent;
        price = price;
        rentalPeriod = ?#Monthly;
        listedAt = now;
        featuredUntil = null;
      };
      listings.put(propertyId, listing);
    };

    return propertyId;
  };

  // Get property by ID
  public query func getProperty(id : PropertyId) : async ?Property {
    return properties.get(id);
  };

  // Get all properties owned by the caller
  public shared(msg) func getMyProperties() : async [Property] {
    let caller = msg.caller;

    switch (userProperties.get(caller)) {
      case (null) { return []; };
      case (?propertyIds) {
        var result : [Property] = [];
        for (id in propertyIds.vals()) {
          switch (properties.get(id)) {
            case (null) {};
            case (?property) {
              result := Array.append(result, [property]);
            };
          };
        };
        return result;
      };
    };
  };

  // Get all property listings
  public query func getAllListings() : async [PropertyListing] {
    return Iter.toArray(listings.vals());
  };

  // Update property details
  public shared(msg) func updateProperty(
    id : PropertyId,
    address : ?Text,
    description : ?Text,
    price : ?Nat,
    isForSale : ?Bool,
    isForRent : ?Bool,
    images : ?[Text]
  ) : async Bool {
    let caller = msg.caller;

    switch (properties.get(id)) {
      case (null) { return false; };
      case (?property) {
        if (property.owner != caller) {
          return false; // Only the owner can update
        };

        let updatedProperty : Property = {
          id = property.id;
          owner = property.owner;
          address = Option.get(address, property.address);
          description = Option.get(description, property.description);
          price = Option.get(price, property.price);
          isForSale = Option.get(isForSale, property.isForSale);
          isForRent = Option.get(isForRent, property.isForRent);
          images = Option.get(images, property.images);
          createdAt = property.createdAt;
          updatedAt = Time.now();
        };

        properties.put(id, updatedProperty);

        // Update listing if needed
        if (Option.isSome(isForSale) or Option.isSome(isForRent) or Option.isSome(price)) {
          if (updatedProperty.isForSale) {
            let listing : PropertyListing = {
              property = updatedProperty;
              listingType = #Sale;
              price = updatedProperty.price;
              rentalPeriod = null;
              listedAt = Time.now();
              featuredUntil = null;
            };
            listings.put(id, listing);
          } else if (updatedProperty.isForRent) {
            let listing : PropertyListing = {
              property = updatedProperty;
              listingType = #Rent;
              price = updatedProperty.price;
              rentalPeriod = ?#Monthly;
              listedAt = Time.now();
              featuredUntil = null;
            };
            listings.put(id, listing);
          } else {
            listings.delete(id);
          };
        };

        return true;
      };
    };
  };

  // Search properties by criteria
  public query func searchProperties(searchTerm : Text) : async [Property] {
    var results : [Property] = [];

    for (property in properties.vals()) {
      if (
        Text.contains(Text.toLowercase(property.address), #text(Text.toLowercase(searchTerm))) or
        Text.contains(Text.toLowercase(property.description), #text(Text.toLowercase(searchTerm)))
      ) {
        results := Array.append(results, [property]);
      };
    };

    return results;
  };
}