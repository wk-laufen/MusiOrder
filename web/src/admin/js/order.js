$(function() {
	$('.sortable').sortable({
	  handle: 'span.grade',
		update: function(ev, ui) {
			Order.updateGrade(ui.item);
		}
	}).disableSelection();
	
	var $tabItems = $('#Tabs ul:first li').droppable({
		accept: '.article',
		tolerance: 'pointer',
		drop: function(ev, ui) {
			var $item = $(this);
			var $list = $($item.find('a').attr('href')).find('.sortable');
			ui.draggable.hide('slow', function() {
				$('#Tabs').tabs('select', $tabItems.index($item));
				$(this).appendTo($list).show('slow');
				$(this).find('[name$="][id_group]"]').val($item.find('input[type="hidden"][name="groupId"]').val());
				Order.updateGrade($(this));
			});
		}
	});
	
	$('#SendBill').dialog({
		autoOpen: false,
		bgiframe: true,
		modal: true,
		width: 500,
		height: 250,
		buttons: {
			'Senden': function() {
				$(this).find('form').ajaxSubmit({
					'dataType': 'json',
					'success': function(ret) {
						var options = {
							width: 750,
							buttons: {
								OK: function() {
									$(this).dialog('close');
									if(ret.code == 0) {
										return;
									}
									$('.ui-dialog-content').each(function() {
										if($(this).dialog('isOpen')) {
											$(this).dialog('close');
										}
									});
								}
							}
						};
						Order.alert('Rechnungen versenden', ret.message, options);
					}
				});
			},
			'Abbrechen': function() {
				$(this).dialog('close');
			}
		}
	});
});

$.extend(Order, {
	'initOrders': function() {
		var $this = this;
		$('#Orders').dialog({
			autoOpen: false,
			bgiframe: true,
			modal: true,
			width: 500,
			height: 500,
			open: function() {$(this).empty();},
			close: function() { $this.resetUser(); },
			buttons: {
				'Schließen': function() {
					$(this).dialog('close');
				},
				'Drucken': function() {
					$('<a></a>').attr({'target': '_blank'}).bind('click', function() {
						window.open("print.php?user[id]=" + user.id);
					}).trigger('click');
				}
			}
		});
	},
	
	'updateGrade': function($article) {
		var $articles = $article.parent().find('.article');
		$articles.each(function() {
			var grade = parseInt($articles.index(this));
			$(this).find('[name$="][grade]"]').val(grade + 1);
		});
	},
	
	'toggleState': function(articleIndex) {
		var $state = $('#Article-' + articleIndex + ' [name$="][state]"]');
		var $attribs;
		if($state.val() == "0") {
			$state.val("1");
			$attribs = {'title': 'Online', 'alt': 'Online'};
		} else {
			$state.val("0");
			$attribs = {'title': 'Offline', 'alt': 'Offline'};
		}
		$attribs.src = `${BASE_DIR}/images/state_${$state.val()}.png`;
		$('#Article-' + articleIndex + ' .state img').attr($attribs);
	},
	
	'toggleTrash': function(articleIndex) {
		var $trash = $('#Article-' + articleIndex + ' [name$="][trash]"]');
		var $attribs;
		if($trash.val() == "0") {
			$trash.val("1");
			$attribs = {'title': 'Artikel löschen', 'alt': 'Artikel löschen'};
		} else {
			$trash.val("0");
			$attribs = {'title': 'Artikel nicht löschen', 'alt': 'Artikel nicht löschen'};
		}
		$attribs.src = `${BASE_DIR}/images/trash_${$trash.val()}.png`;
		$('#Article-' + articleIndex + ' .trash img').attr($attribs);
	},
	
	'selectUser': function(userId) {
		user.id = userId;
		var action = $('#Users input[name="do"]').val();
		if(action == 'showOrders') {
			Order.showOrdersForUser();
		} else if(action == 'chooseUsersForBill') {
			$('#User-' + user.id).toggleClass('selected');
		}
	},
	
	'chooseUsersForBill': function() {
		$('#Users input[name="do"]').attr('value', 'chooseUsersForBill');
		$('#Users').dialog('option', 'buttons', {
			Abbrechen: function() {
				$(this).dialog('close');
			},
			OK: function() {
				var userIds = new Array();
				$(this).find('.user.selected').each(function() {
					var userId = $(this).attr('id').split('-');
					userId = userId[userId.length - 1]
					userIds.push(userId);
				});
				$('#cntSelectedUser').text(userIds.length);
				if(userIds.length > 0) {
					userIds = userIds.join(',');
				} else {
					userIds = '';
				};
				$('#SendBill input[name="bill[recipientIds]"]').val(userIds);
				$('#SendBillToSeveral').attr('checked', 'checked');
				$(this).dialog('close');
			}
		}).dialog('open');
		var userIds = $('#SendBill input[name="bill[recipientIds]"]').val().split(',');
		for(var i in userIds) {
			$('#Users #User-' + userIds[i]).addClass('selected');
		}
	},
	
	'sendBill': function() {
		$('#SendBill').dialog('open');
	},
	
	'addArticle': function(groupId) {
		var $article = $('#Tabs div.article:first').clone(true);
		var $group = $('#Tab-' + groupId + ' .sortable');
		var oldIndex = $article.attr('id').split('-');
		oldIndex = oldIndex[oldIndex.length - 1];
		var newIndex = $('#Tabs div.article').length;
		$article.find('input').each(function() {
			$(this).attr('name', $(this).attr('name').replace(new RegExp('\[(' + oldIndex  + ')\]'), newIndex));
		});
		$article.attr('id', 'Article-' + newIndex);
		$article.find('input[name$="][id]"]').val('');
		$article.find('input[name$="][id_group]"]').val(groupId);
		$article.find('input[name$="][state]"]').val('0');
		$article.find('input[name$="][trash]"]').val('0');
		$article.find('input[name$="][grade]"]').val($group.find('.article').length + 1);
		$article.find('input[name$="][name]"]').val('');
		$article.find('input[name$="][price]"]').val('');
		$article.find('.state').attr('href', 'javascript:Order.toggleState(' + newIndex + ')');
		$article.find('.state img').attr({'title': 'Offline', 'alt': 'Offline', 'src': `${BASE_DIR}/images/state_0.png`});
		$article.find('.trash').attr('href', 'javascript:Order.toggleTrash(' + newIndex + ')').hide();
		$article.find('.trash img').attr({'title': 'Nicht löschen', 'alt': 'Nicht löschen', 'src': `${BASE_DIR}/images/trash_0.png`});
		$group.append($article);
	}
});
