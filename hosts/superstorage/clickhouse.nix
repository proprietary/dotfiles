{ config, lib, ... }:
{
  services.my-clickhouse = {
    enable = true;
  };
  environment.etc = {
    "clickhouse-server/config.d/01-data-dir.xml".text = ''
      <clickhouse>
        <path>/superstorage/clickhouse/</path>
      </clickhouse>
    '';
    "clickhouse-server/users.d/02-enable-sql-user-mode.xml".text = ''
      <clickhouse>
        <users>
          <default>
            <access_management>1</access_management>
            <named_collection_control>1</named_collection_control>
            <show_named_collections>1</show_named_collections>
            <show_named_collections_secrets>1</show_named_collections_secrets>
          </default>
        </users>
      </clickhouse>
    '';
  };
}
