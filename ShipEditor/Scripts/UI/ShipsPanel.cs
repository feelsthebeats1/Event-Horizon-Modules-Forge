using UnityEngine;
using ShipEditor.Model;

namespace ShipEditor.UI
{
    public class ShipsPanel : MonoBehaviour
    {
		private IShipEditorModel _shipEditor;
		
		[SerializeField] private ShipListContentFiller _contentFiller;
        [SerializeField] private ListScrollRect _shipList;

		public void Initialize(IShipEditorModel shipEditor)
		{
			_shipEditor = shipEditor;
			_contentFiller.Initialize(_shipEditor.Inventory.Ships);
			_shipList.RefreshContent();
		}

		public void OnShipClicked(ShipItem item)
		{
			_shipEditor.SelectShip(item.Ship);
		}

		public bool Visible
		{
			get => gameObject.activeSelf;
			set => gameObject.SetActive(value);
		}
	}
}
